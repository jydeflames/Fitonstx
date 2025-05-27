;; User Identity & Activity Verification Contract
;; Manages user registration, activity submission, and verification through voting

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1000))
(define-constant ERR_USER_EXISTS (err u1001))
(define-constant ERR_USER_NOT_FOUND (err u1002))
(define-constant ERR_ACTIVITY_EXISTS (err u1003))
(define-constant ERR_ACTIVITY_NOT_FOUND (err u1004))
(define-constant ERR_INVALID_PROOF (err u1005))
(define-constant ERR_ALREADY_VOTED (err u1006))
(define-constant ERR_VERIFICATION_COMPLETE (err u1007))
(define-constant ERR_INSUFFICIENT_VOTES (err u1008))

;; Data Variables
(define-data-var voting-threshold uint u3) ;; Minimum votes needed for verification
(define-data-var activity-counter uint u0)

;; Data Maps
;; User registry mapping wallet address to user metadata
(define-map users
  principal
  {
    registered-at: uint,
    metadata-hash: (buff 32),
    reputation-score: uint,
    total-activities: uint,
    verified-activities: uint
  }
)

;; Activity submissions
(define-map activities
  uint ;; activity-id
  {
    submitter: principal,
    encrypted-data-hash: (buff 32),
    zkp-proof-hash: (buff 32),
    submitted-at: uint,
    verification-status: (string-ascii 20), ;; "pending", "verified", "rejected"
    total-votes: uint,
    approve-votes: uint,
    reject-votes: uint
  }
)

;; Voting tracking to prevent double voting
(define-map activity-votes
  {activity-id: uint, voter: principal}
  {vote: bool, voted-at: uint} ;; true = approve, false = reject
)

;; Oracle/DAO members authorized to vote
(define-map authorized-oracles
  principal
  {authorized: bool, authorized-at: uint}
)

;; Read-only functions

;; Get user information
(define-read-only (get-user (wallet principal))
  (map-get? users wallet)
)

;; Get activity information
(define-read-only (get-activity (activity-id uint))
  (map-get? activities activity-id)
)

;; Check if user is registered
(define-read-only (is-user-registered (wallet principal))
  (is-some (map-get? users wallet))
)

;; Check if oracle is authorized
(define-read-only (is-oracle-authorized (oracle principal))
  (match (map-get? authorized-oracles oracle)
    oracle-data (get authorized oracle-data)
    false
  )
)

;; Get voting status for specific activity and voter
(define-read-only (get-vote-status (activity-id uint) (voter principal))
  (map-get? activity-votes {activity-id: activity-id, voter: voter})
)

;; Get current voting threshold
(define-read-only (get-voting-threshold)
  (var-get voting-threshold)
)

;; Get current activity counter
(define-read-only (get-activity-counter)
  (var-get activity-counter)
)

;; Public functions

;; Register a new user
(define-public (register-user (metadata-hash (buff 32)))
  (let (
    (caller tx-sender)
    (current-block stacks-block-height)
  )
    ;; Check if user already exists
    (asserts! (not (is-user-registered caller)) ERR_USER_EXISTS)
    
    ;; Register the user
    (map-set users caller {
      registered-at: current-block,
      metadata-hash: metadata-hash,
      reputation-score: u0,
      total-activities: u0,
      verified-activities: u0
    })
    
    (ok true)
  )
)

;; Submit activity with encrypted data and ZKP proof
(define-public (submit-activity (encrypted-data-hash (buff 32)) (zkp-proof-hash (buff 32)))
  (let (
    (caller tx-sender)
    (current-block stacks-block-height)
    (new-activity-id (+ (var-get activity-counter) u1))
  )
    ;; Check if user is registered
    (asserts! (is-user-registered caller) ERR_USER_NOT_FOUND)
    
    ;; Basic ZKP proof validation (simplified - in practice would verify cryptographic proof)
    (asserts! (> (len zkp-proof-hash) u0) ERR_INVALID_PROOF)
    
    ;; Create activity record
    (map-set activities new-activity-id {
      submitter: caller,
      encrypted-data-hash: encrypted-data-hash,
      zkp-proof-hash: zkp-proof-hash,
      submitted-at: current-block,
      verification-status: "pending",
      total-votes: u0,
      approve-votes: u0,
      reject-votes: u0
    })
    
    ;; Update activity counter
    (var-set activity-counter new-activity-id)
    
    ;; Update user's total activities
    (match (map-get? users caller)
      user-data (map-set users caller (merge user-data {
        total-activities: (+ (get total-activities user-data) u1)
      }))
      false ;; Should not happen due to earlier check
    )
    
    (ok new-activity-id)
  )
)

;; Vote on activity verification (for oracles/DAO members)
(define-public (vote-on-activity (activity-id uint) (approve bool))
  (let (
    (caller tx-sender)
    (current-block stacks-block-height)
  )
    ;; Check if caller is authorized oracle
    (asserts! (is-oracle-authorized caller) ERR_UNAUTHORIZED)
    
    ;; Check if activity exists
    (asserts! (is-some (map-get? activities activity-id)) ERR_ACTIVITY_NOT_FOUND)
    
    ;; Check if already voted
    (asserts! (is-none (map-get? activity-votes {activity-id: activity-id, voter: caller})) ERR_ALREADY_VOTED)
    
    ;; Get activity data
    (match (map-get? activities activity-id)
      activity-data
      (begin
        ;; Check if verification is still pending
        (asserts! (is-eq (get verification-status activity-data) "pending") ERR_VERIFICATION_COMPLETE)
        
        ;; Record the vote
        (map-set activity-votes {activity-id: activity-id, voter: caller} {
          vote: approve,
          voted-at: current-block
        })
        
        ;; Update activity vote counts
        (let (
          (new-total-votes (+ (get total-votes activity-data) u1))
          (new-approve-votes (if approve (+ (get approve-votes activity-data) u1) (get approve-votes activity-data)))
          (new-reject-votes (if approve (get reject-votes activity-data) (+ (get reject-votes activity-data) u1)))
        )
          (map-set activities activity-id (merge activity-data {
            total-votes: new-total-votes,
            approve-votes: new-approve-votes,
            reject-votes: new-reject-votes
          }))
          
          ;; Check if we have enough votes to finalize
          (if (>= new-total-votes (var-get voting-threshold))
            (finalize-verification activity-id new-approve-votes new-reject-votes)
            (ok true)
          )
        )
      )
      ERR_ACTIVITY_NOT_FOUND
    )
  )
)

;; Internal function to finalize verification
(define-private (finalize-verification (activity-id uint) (approve-votes uint) (reject-votes uint))
  (match (map-get? activities activity-id)
    activity-data
    (let (
      (is-approved (> approve-votes reject-votes))
      (new-status (if is-approved "verified" "rejected"))
      (submitter (get submitter activity-data))
    )
      ;; Update activity status
      (map-set activities activity-id (merge activity-data {
        verification-status: new-status
      }))
      
      ;; Update user reputation and verified activities count if approved
      (if is-approved
        (match (map-get? users submitter)
          user-data (map-set users submitter (merge user-data {
            reputation-score: (+ (get reputation-score user-data) u10),
            verified-activities: (+ (get verified-activities user-data) u1)
          }))
          false
        )
        true
      )
      
      (ok is-approved)
    )
    ERR_ACTIVITY_NOT_FOUND
  )
)

;; Admin functions

;; Add authorized oracle (only contract owner)
(define-public (add-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-oracles oracle {
      authorized: true,
      authorized-at: stacks-block-height
    })
    (ok true)
  )
)

;; Remove oracle authorization (only contract owner)
(define-public (remove-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-oracles oracle {
      authorized: false,
      authorized-at: stacks-block-height
    })
    (ok true)
  )
)

;; Update voting threshold (only contract owner)
(define-public (set-voting-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-threshold u0) (err u2000))
    (var-set voting-threshold new-threshold)
    (ok true)
  )
)

;; Force finalize verification (emergency function - only contract owner)
(define-public (force-finalize-verification (activity-id uint) (approve bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? activities activity-id)) ERR_ACTIVITY_NOT_FOUND)
    
    (match (map-get? activities activity-id)
      activity-data
      (let (
        (new-status (if approve "verified" "rejected"))
        (submitter (get submitter activity-data))
      )
        ;; Update activity status
        (map-set activities activity-id (merge activity-data {
          verification-status: new-status
        }))
        
        ;; Update user reputation if approved
        (if approve
          (match (map-get? users submitter)
            user-data (map-set users submitter (merge user-data {
              reputation-score: (+ (get reputation-score user-data) u10),
              verified-activities: (+ (get verified-activities user-data) u1)
            }))
            false
          )
          true
        )
        
        (ok approve)
      )
      ERR_ACTIVITY_NOT_FOUND
    )
  )
)