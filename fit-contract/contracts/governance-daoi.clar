;; Governance DAO Smart Contract
;; Manages proposals, voting, and execution for FIT token holders

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u101))
(define-constant ERR-PROPOSAL-EXPIRED (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-INSUFFICIENT-STAKE (err u104))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u105))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u106))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u107))

;; Configuration constants
(define-constant VOTING-PERIOD u144) ;; ~24 hours in blocks (assuming 10min blocks)
(define-constant MIN-PROPOSAL-STAKE u1000000) ;; 1 FIT token (6 decimals)
(define-constant QUORUM-THRESHOLD u20) ;; 20% of total staked tokens
(define-constant APPROVAL-THRESHOLD u51) ;; 51% approval needed

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var total-staked-tokens uint u0)

;; Data Maps
(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    metadata: (string-ascii 500),
    action-type: (string-ascii 50),
    proposer: principal,
    start-block: uint,
    end-block: uint,
    votes-for: uint,
    votes-against: uint,
    total-votes: uint,
    executed: bool,
    stake-amount: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  {
    support: bool,
    voting-power: uint,
    block-height: uint
  }
)

(define-map user-stakes
  { user: principal }
  { staked-amount: uint }
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
  (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-user-stake (user principal))
  (default-to u0 (get staked-amount (map-get? user-stakes { user: user })))
)

(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

(define-read-only (get-total-staked)
  (var-get total-staked-tokens)
)

(define-read-only (is-proposal-active (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (and 
      (>= stacks-block-height (get start-block proposal))
      (<= stacks-block-height (get end-block proposal))
      (not (get executed proposal))
    )
    false
  )
)

(define-read-only (is-proposal-passed (proposal-id uint))
  (match (get-proposal proposal-id)
    proposal (let
      (
        (total-votes (get total-votes proposal))
        (votes-for (get votes-for proposal))
        (quorum-met (>= total-votes (/ (* (var-get total-staked-tokens) QUORUM-THRESHOLD) u100)))
        (approval-met (>= (* votes-for u100) (* total-votes APPROVAL-THRESHOLD)))
      )
      (and quorum-met approval-met (> stacks-block-height (get end-block proposal)))
    )
    false
  )
)

;; Public functions

(define-public (stake-tokens (amount uint))
  (let
    (
      (current-stake (get-user-stake tx-sender))
      (new-stake (+ current-stake amount))
    )
    ;; Transfer tokens to contract (assuming FIT token contract exists)
    ;; (try! (contract-call? .fit-token transfer amount tx-sender (as-contract tx-sender) none))
    
    ;; Update user stake
    (map-set user-stakes { user: tx-sender } { staked-amount: new-stake })
    
    ;; Update total staked
    (var-set total-staked-tokens (+ (var-get total-staked-tokens) amount))
    
    (ok new-stake)
  )
)

(define-public (unstake-tokens (amount uint))
  (let
    (
      (current-stake (get-user-stake tx-sender))
    )
    (asserts! (>= current-stake amount) ERR-INSUFFICIENT-STAKE)
    
    ;; Update user stake
    (map-set user-stakes { user: tx-sender } { staked-amount: (- current-stake amount) })
    
    ;; Update total staked
    (var-set total-staked-tokens (- (var-get total-staked-tokens) amount))
    
    ;; Transfer tokens back to user
    ;; (try! (as-contract (contract-call? .fit-token transfer amount tx-sender tx-sender none)))
    
    (ok (- current-stake amount))
  )
)

(define-public (create-proposal (title (string-ascii 100)) (metadata (string-ascii 500)) (action-type (string-ascii 50)))
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (user-stake (get-user-stake tx-sender))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height VOTING-PERIOD))
    )
    ;; Check minimum stake requirement
    (asserts! (>= user-stake MIN-PROPOSAL-STAKE) ERR-INSUFFICIENT-STAKE)
    
    ;; Create proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        metadata: metadata,
        action-type: action-type,
        proposer: tx-sender,
        start-block: start-block,
        end-block: end-block,
        votes-for: u0,
        votes-against: u0,
        total-votes: u0,
        executed: false,
        stake-amount: user-stake
      }
    )
    
    ;; Update proposal counter
    (var-set proposal-counter proposal-id)
    
    (ok proposal-id)
  )
)

(define-public (vote (proposal-id uint) (support bool))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
      (user-stake (get-user-stake tx-sender))
      (existing-vote (get-vote proposal-id tx-sender))
    )
    ;; Check if proposal is active
    (asserts! (is-proposal-active proposal-id) ERR-PROPOSAL-EXPIRED)
    
    ;; Check if user has stake
    (asserts! (> user-stake u0) ERR-INSUFFICIENT-STAKE)
    
    ;; Check if user hasn't voted yet
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    
    ;; Record vote
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      {
        support: support,
        voting-power: user-stake,
        block-height: stacks-block-height
      }
    )
    
    ;; Update proposal vote counts
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal {
        votes-for: (if support 
          (+ (get votes-for proposal) user-stake)
          (get votes-for proposal)
        ),
        votes-against: (if support
          (get votes-against proposal)
          (+ (get votes-against proposal) user-stake)
        ),
        total-votes: (+ (get total-votes proposal) user-stake)
      })
    )
    
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
    )
    ;; Check if voting period has ended
    (asserts! (> stacks-block-height (get end-block proposal)) ERR-VOTING-PERIOD-ACTIVE)
    
    ;; Check if proposal hasn't been executed
    (asserts! (not (get executed proposal)) ERR-PROPOSAL-ALREADY-EXECUTED)
    
    ;; Check if proposal passed
    (asserts! (is-proposal-passed proposal-id) ERR-PROPOSAL-NOT-PASSED)
    
    ;; Mark as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    
    ;; Execute proposal logic based on action-type
    ;; (try! (execute-proposal-action proposal-id (get action-type proposal)))
    
    (ok true)
  )
)

;; Private functions

(define-private (execute-proposal-action (proposal-id uint) (action-type (string-ascii 50)))
  (begin
    ;; This is where you would implement specific proposal execution logic
    ;; For example:
    ;; - "add-pool": Add new liquidity pool
    ;; - "update-yield": Update yield strategy
    ;; - "partnership": Execute partnership agreement
    ;; - "feature": Enable new feature
    
    ;; For now, we'll just emit a print statement
    (print { 
      event: "proposal-executed", 
      proposal-id: proposal-id, 
      action-type: action-type 
    })
    (ok true)
  )
)

;; Admin functions (for initial setup)

(define-public (set-total-staked (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set total-staked-tokens amount)
    (ok true)
  )
)
