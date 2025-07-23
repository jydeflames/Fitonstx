;; FIT Token Economy Smart Contract
;; Implements SIP-010 fungible token standard with fitness activity rewards

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-TOKEN-OWNER (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-ALREADY-CLAIMED (err u104))
(define-constant ERR-NOT-ELIGIBLE (err u105))
(define-constant ERR-UNAUTHORIZED (err u106))

;; Token constants
(define-constant TOKEN-NAME "FIT Token")
(define-constant TOKEN-SYMBOL "FIT")
(define-constant TOKEN-DECIMALS u6)
(define-constant TOKEN-URI u"https://fit-token.com/metadata")

;; Data variables
(define-data-var token-total-supply uint u0)
(define-data-var contract-paused bool false)

;; Data maps
(define-map token-balances principal uint)
(define-map token-supplies principal uint)

;; Activity verification system
(define-map verified-activities 
  { user: principal, activity-id: uint } 
  { verified: bool, amount: uint, timestamp: uint }
)

;; Bonus tracking
(define-map user-streaks principal uint)
(define-map user-milestones principal uint)
(define-map user-referrals principal uint)
(define-map claimed-bonuses 
  { user: principal, bonus-type: (string-ascii 20), period: uint }
  bool
)

;; Admin addresses for activity verification
(define-map authorized-verifiers principal bool)

;; Bonus rates (in micro-FIT tokens)
(define-data-var streak-bonus-rate uint u1000000) ;; 1 FIT per day streak
(define-data-var milestone-bonus-rate uint u5000000) ;; 5 FIT per milestone
(define-data-var referral-bonus-rate uint u10000000) ;; 10 FIT per referral

;; SIP-010 Implementation

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
  (begin
    (asserts! (not (var-get contract-paused)) (err u107))
    (asserts! (is-eq tx-sender sender) ERR-NOT-TOKEN-OWNER)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let ((sender-balance (ft-get-balance fit-token sender)))
      (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
      (try! (ft-transfer? fit-token amount sender recipient))
      (print {
        action: "transfer",
        sender: sender,
        recipient: recipient,
        amount: amount,
        memo: memo
      })
      (ok true)
    )
  )
)

(define-read-only (get-name)
  (ok TOKEN-NAME)
)

(define-read-only (get-symbol)
  (ok TOKEN-SYMBOL)
)

(define-read-only (get-decimals)
  (ok TOKEN-DECIMALS)
)

(define-read-only (get-balance (who principal))
  (ok (ft-get-balance fit-token who))
)

(define-read-only (get-total-supply)
  (ok (ft-get-supply fit-token))
)

(define-read-only (get-token-uri)
  (ok (some TOKEN-URI))
)

;; Define the fungible token
(define-fungible-token fit-token)

;; Core FIT Token Functions

(define-public (mint-fit (user-id principal) (amount uint))
  (begin
    (asserts! (not (var-get contract-paused)) (err u107))
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (default-to false (map-get? authorized-verifiers tx-sender))) 
              ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (try! (ft-mint? fit-token amount user-id))
    (print {
      action: "mint",
      user: user-id,
      amount: amount,
      minter: tx-sender
    })
    (ok true)
  )
)

(define-public (burn-fit (user-id principal) (amount uint))
  (begin
    (asserts! (not (var-get contract-paused)) (err u107))
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (is-eq tx-sender user-id)
                  (default-to false (map-get? authorized-verifiers tx-sender))) 
              ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (let ((user-balance (ft-get-balance fit-token user-id)))
      (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
      (try! (ft-burn? fit-token amount user-id))
      (print {
        action: "burn",
        user: user-id,
        amount: amount,
        burner: tx-sender
      })
      (ok true)
    )
  )
)

;; Activity Verification System

(define-public (verify-activity (user principal) (activity-id uint) (reward-amount uint))
  (begin
    (asserts! (not (var-get contract-paused)) (err u107))
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (default-to false (map-get? authorized-verifiers tx-sender))) 
              ERR-UNAUTHORIZED)
    (asserts! (> reward-amount u0) ERR-INVALID-AMOUNT)
    
    ;; Record the verified activity
    (map-set verified-activities
      { user: user, activity-id: activity-id }
      { verified: true, amount: reward-amount, timestamp: stacks-block-height }
    )
    
    ;; Mint tokens as reward
    (try! (ft-mint? fit-token reward-amount user))
    
    ;; Update user streak
    (map-set user-streaks user 
      (+ (default-to u0 (map-get? user-streaks user)) u1))
    
    (print {
      action: "activity-verified",
      user: user,
      activity-id: activity-id,
      reward: reward-amount,
      verifier: tx-sender
    })
    (ok true)
  )
)

;; Bonus Claim System

(define-public (claim-streak-bonus (user-id principal))
  (let (
    (current-streak (default-to u0 (map-get? user-streaks user-id)))
    (bonus-key { user: user-id, bonus-type: "streak", period: (/ stacks-block-height u144) }) ;; Daily periods
    (bonus-amount (* current-streak (var-get streak-bonus-rate)))
  )
    (asserts! (not (var-get contract-paused)) (err u107))
    (asserts! (is-eq tx-sender user-id) ERR-UNAUTHORIZED)
    (asserts! (> current-streak u0) ERR-NOT-ELIGIBLE)
    (asserts! (is-none (map-get? claimed-bonuses bonus-key)) ERR-ALREADY-CLAIMED)
    
    ;; Mark bonus as claimed
    (map-set claimed-bonuses bonus-key true)
    
    ;; Mint bonus tokens
    (try! (ft-mint? fit-token bonus-amount user-id))
    
    (print {
      action: "streak-bonus-claimed",
      user: user-id,
      streak: current-streak,
      bonus: bonus-amount
    })
    (ok bonus-amount)
  )
)

(define-public (claim-milestone-bonus (user-id principal) (milestone uint))
  (let (
    (user-milestones-id (default-to u0 (map-get? user-milestones user-id)))
    (bonus-key { user: user-id, bonus-type: "milestone", period: milestone })
    (bonus-amount (var-get milestone-bonus-rate))
  )
    (asserts! (not (var-get contract-paused)) (err u107))
    (asserts! (is-eq tx-sender user-id) ERR-UNAUTHORIZED)
    (asserts! (>= user-milestones-id milestone) ERR-NOT-ELIGIBLE)
    (asserts! (is-none (map-get? claimed-bonuses bonus-key)) ERR-ALREADY-CLAIMED)
    
    ;; Mark bonus as claimed
    (map-set claimed-bonuses bonus-key true)
    
    ;; Mint bonus tokens
    (try! (ft-mint? fit-token bonus-amount user-id))
    
    (print {
      action: "milestone-bonus-claimed",
      user: user-id,
      milestone: milestone,
      bonus: bonus-amount
    })
    (ok bonus-amount)
  )
)

(define-public (claim-referral-bonus (user-id principal))
  (let (
    (referral-count (default-to u0 (map-get? user-referrals user-id)))
    (bonus-key { user: user-id, bonus-type: "referral", period: (/ stacks-block-height u1008) }) ;; Weekly periods
    (bonus-amount (* referral-count (var-get referral-bonus-rate)))
  )
    (asserts! (not (var-get contract-paused)) (err u107))
    (asserts! (is-eq tx-sender user-id) ERR-UNAUTHORIZED)
    (asserts! (> referral-count u0) ERR-NOT-ELIGIBLE)
    (asserts! (is-none (map-get? claimed-bonuses bonus-key)) ERR-ALREADY-CLAIMED)
    
    ;; Mark bonus as claimed
    (map-set claimed-bonuses bonus-key true)
    
    ;; Mint bonus tokens
    (try! (ft-mint? fit-token bonus-amount user-id))
    
    ;; Reset referral count after claiming
    (map-set user-referrals user-id u0)
    
    (print {
      action: "referral-bonus-claimed",
      user: user-id,
      referrals: referral-count,
      bonus: bonus-amount
    })
    (ok bonus-amount)
  )
)

;; Admin Functions

(define-public (add-authorized-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set authorized-verifiers verifier true)
    (print { action: "verifier-added", verifier: verifier })
    (ok true)
  )
)

(define-public (remove-authorized-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-delete authorized-verifiers verifier)
    (print { action: "verifier-removed", verifier: verifier })
    (ok true)
  )
)

(define-public (update-milestone (user principal) (milestone-count uint))
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (default-to false (map-get? authorized-verifiers tx-sender))) 
              ERR-UNAUTHORIZED)
    (map-set user-milestones user milestone-count)
    (ok true)
  )
)

(define-public (add-referral (referrer principal))
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (default-to false (map-get? authorized-verifiers tx-sender))) 
              ERR-UNAUTHORIZED)
    (map-set user-referrals referrer 
      (+ (default-to u0 (map-get? user-referrals referrer)) u1))
    (ok true)
  )
)

(define-public (set-bonus-rates (streak-rate uint) (milestone-rate uint) (referral-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set streak-bonus-rate streak-rate)
    (var-set milestone-bonus-rate milestone-rate)
    (var-set referral-bonus-rate referral-rate)
    (ok true)
  )
)

(define-public (pause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set contract-paused true)
    (ok true)
  )
)

(define-public (unpause-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set contract-paused false)
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-user-streak (user principal))
  (default-to u0 (map-get? user-streaks user))
)

(define-read-only (get-user-milestones (user principal))
  (default-to u0 (map-get? user-milestones user))
)

(define-read-only (get-user-referrals (user principal))
  (default-to u0 (map-get? user-referrals user))
)

(define-read-only (is-bonus-claimed (user principal) (bonus-type (string-ascii 20)) (period uint))
  (default-to false (map-get? claimed-bonuses { user: user, bonus-type: bonus-type, period: period }))
)

(define-read-only (is-authorized-verifier (verifier principal))
  (default-to false (map-get? authorized-verifiers verifier))
)

(define-read-only (get-activity-verification (user principal) (activity-id uint))
  (map-get? verified-activities { user: user, activity-id: activity-id })
)

(define-read-only (get-bonus-rates)
  {
    streak-rate: (var-get streak-bonus-rate),
    milestone-rate: (var-get milestone-bonus-rate),
    referral-rate: (var-get referral-bonus-rate)
  }
)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

;; Initialize contract
(begin
  (map-set authorized-verifiers CONTRACT-OWNER true)
  (print { action: "contract-initialized", owner: CONTRACT-OWNER })
)