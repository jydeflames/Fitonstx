;; Asset Pools & Ownership Smart Contract
;; Enables tokenization of real-world and digital assets

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-POOL-INACTIVE (err u104))
(define-constant ERR-INSUFFICIENT-SHARES (err u105))
(define-constant ERR-ZERO-AMOUNT (err u106))
(define-constant ERR-INVALID-PRICE (err u107))

;; Data Variables
(define-data-var next-pool-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% in basis points

;; Asset Pool Structure
(define-map asset-pools
  { pool-id: uint }
  {
    creator: principal,
    asset-name: (string-ascii 64),
    asset-description: (string-ascii 256),
    asset-type: (string-ascii 32),
    token-price: uint, ;; Price per share in $FIT
    max-shares: uint,
    current-shares: uint,
    total-yield-distributed: uint,
    is-active: bool,
    created-at: uint
  }
)

;; User Holdings - tracks how many shares each user owns in each pool
(define-map user-holdings
  { user: principal, pool-id: uint }
  { shares: uint, last-yield-claim: uint }
)

;; Pool Yield Data - tracks yield distribution per pool
(define-map pool-yield-data
  { pool-id: uint }
  { 
    total-yield-available: uint,
    yield-per-share: uint,
    last-distribution: uint
  }
)

;; Total shares owned by each user across all pools
(define-map user-total-shares
  { user: principal }
  { total-shares: uint }
)

;; FIT Token Contract (assuming it's deployed separately)
(define-trait sip-010-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

;; Read-only functions

(define-read-only (get-pool-info (pool-id uint))
  (map-get? asset-pools { pool-id: pool-id })
)

(define-read-only (get-user-holdings (user principal) (pool-id uint))
  (map-get? user-holdings { user: user, pool-id: pool-id })
)

(define-read-only (get-pool-yield-data (pool-id uint))
  (map-get? pool-yield-data { pool-id: pool-id })
)

(define-read-only (get-user-total-shares (user principal))
  (default-to { total-shares: u0 } (map-get? user-total-shares { user: user }))
)

(define-read-only (get-next-pool-id)
  (var-get next-pool-id)
)

(define-read-only (calculate-shares-value (user principal) (pool-id uint))
  (let (
    (holdings (get-user-holdings user pool-id))
    (pool-info (get-pool-info pool-id))
  )
    (match holdings
      holding-data
        (match pool-info
          pool-data
            (ok (* (get shares holding-data) (get token-price pool-data)))
          ERR-NOT-FOUND
        )
      ERR-NOT-FOUND
    )
  )
)

;; Private functions

(define-private (update-user-total-shares (user principal) (shares-delta int))
  (let (
    (current-total (get total-shares (get-user-total-shares user)))
    (new-total (if (> shares-delta 0)
                  (+ current-total (to-uint shares-delta))
                  (- current-total (to-uint (- shares-delta)))))
  )
    (map-set user-total-shares
      { user: user }
      { total-shares: new-total }
    )
  )
)

;; Public functions

;; Create a new asset pool
(define-public (create-asset-pool 
  (asset-name (string-ascii 64))
  (asset-description (string-ascii 256))
  (asset-type (string-ascii 32))
  (token-price uint)
  (max-shares uint)
)
  (let (
    (pool-id (var-get next-pool-id))
  )
    (asserts! (> token-price u0) ERR-INVALID-PRICE)
    (asserts! (> max-shares u0) ERR-INVALID-AMOUNT)
    
    ;; Create the asset pool
    (map-set asset-pools
      { pool-id: pool-id }
      {
        creator: tx-sender,
        asset-name: asset-name,
        asset-description: asset-description,
        asset-type: asset-type,
        token-price: token-price,
        max-shares: max-shares,
        current-shares: u0,
        total-yield-distributed: u0,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    ;; Initialize yield data for the pool
    (map-set pool-yield-data
      { pool-id: pool-id }
      {
        total-yield-available: u0,
        yield-per-share: u0,
        last-distribution: stacks-block-height
      }
    )
    
    ;; Increment next pool ID
    (var-set next-pool-id (+ pool-id u1))
    
    (ok pool-id)
  )
)

;; Redeem $FIT tokens for asset shares
(define-public (redeem-for-asset 
  (pool-id uint) 
  (shares-amount uint)
  (fit-token-contract <sip-010-trait>)
)
  (let (
    (pool-info (unwrap! (get-pool-info pool-id) ERR-NOT-FOUND))
    (total-cost (* shares-amount (get token-price pool-info)))
    (current-holdings (default-to { shares: u0, last-yield-claim: stacks-block-height } 
                                 (get-user-holdings tx-sender pool-id)))
    (new-shares (+ (get shares current-holdings) shares-amount))
    (new-current-shares (+ (get current-shares pool-info) shares-amount))
  )
    (asserts! (get is-active pool-info) ERR-POOL-INACTIVE)
    (asserts! (> shares-amount u0) ERR-ZERO-AMOUNT)
    (asserts! (<= new-current-shares (get max-shares pool-info)) ERR-INSUFFICIENT-SHARES)
    
    ;; Transfer $FIT tokens from user to contract
    (try! (contract-call? fit-token-contract transfer 
           total-cost tx-sender (as-contract tx-sender) none))
    
    ;; Update user holdings
    (map-set user-holdings
      { user: tx-sender, pool-id: pool-id }
      { shares: new-shares, last-yield-claim: stacks-block-height }
    )
    
    ;; Update pool current shares
    (map-set asset-pools
      { pool-id: pool-id }
      (merge pool-info { current-shares: new-current-shares })
    )
    
    ;; Update user total shares
    (update-user-total-shares tx-sender (to-int shares-amount))
    
    (ok { shares-purchased: shares-amount, total-cost: total-cost })
  )
)

;; Distribute yield to asset token holders
(define-public (distribute-yield 
  (pool-id uint) 
  (yield-amount uint)
  (fit-token-contract <sip-010-trait>)
)
  (let (
    (pool-info (unwrap! (get-pool-info pool-id) ERR-NOT-FOUND))
    (current-yield-data (default-to 
      { total-yield-available: u0, yield-per-share: u0, last-distribution: stacks-block-height }
      (get-pool-yield-data pool-id)))
    (current-shares (get current-shares pool-info))
    (yield-per-share (if (> current-shares u0) 
                       (/ yield-amount current-shares) 
                       u0))
  )
    ;; Only pool creator or contract owner can distribute yield
    (asserts! (or (is-eq tx-sender (get creator pool-info)) 
                  (is-eq tx-sender CONTRACT-OWNER)) ERR-OWNER-ONLY)
    (asserts! (get is-active pool-info) ERR-POOL-INACTIVE)
    (asserts! (> yield-amount u0) ERR-ZERO-AMOUNT)
    (asserts! (> current-shares u0) ERR-INSUFFICIENT-SHARES)
    
    ;; Transfer yield tokens to contract for distribution
    (try! (contract-call? fit-token-contract transfer 
           yield-amount tx-sender (as-contract tx-sender) none))
    
    ;; Update pool yield data
    (map-set pool-yield-data
      { pool-id: pool-id }
      {
        total-yield-available: (+ (get total-yield-available current-yield-data) yield-amount),
        yield-per-share: (+ (get yield-per-share current-yield-data) yield-per-share),
        last-distribution: stacks-block-height
      }
    )
    
    ;; Update total yield distributed in pool info
    (map-set asset-pools
      { pool-id: pool-id }
      (merge pool-info { 
        total-yield-distributed: (+ (get total-yield-distributed pool-info) yield-amount) 
      })
    )
    
    (ok { yield-distributed: yield-amount, yield-per-share: yield-per-share })
  )
)

;; Claim accumulated yield for a specific pool
(define-public (claim-yield 
  (pool-id uint)
  (fit-token-contract <sip-010-trait>)
)
  (let (
    (holdings-data (unwrap! (get-user-holdings tx-sender pool-id) ERR-NOT-FOUND))
    (yield-data (unwrap! (get-pool-yield-data pool-id) ERR-NOT-FOUND))
    (user-shares (get shares holdings-data))
    (last-claim (get last-yield-claim holdings-data))
    (current-yield-per-share (get yield-per-share yield-data))
    (claimable-yield (* user-shares current-yield-per-share))
  )
    (asserts! (> user-shares u0) ERR-INSUFFICIENT-SHARES)
    (asserts! (> claimable-yield u0) ERR-ZERO-AMOUNT)
    
    ;; Transfer yield to user
    (try! (as-contract (contract-call? fit-token-contract transfer 
           claimable-yield tx-sender tx-sender none)))
    
    ;; Update user's last yield claim
    (map-set user-holdings
      { user: tx-sender, pool-id: pool-id }
      (merge holdings-data { last-yield-claim: stacks-block-height })
    )
    
    (ok claimable-yield)
  )
)

;; Admin function to deactivate a pool
(define-public (deactivate-pool (pool-id uint))
  (let (
    (pool-info (unwrap! (get-pool-info pool-id) ERR-NOT-FOUND))
  )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (is-eq tx-sender (get creator pool-info))) ERR-OWNER-ONLY)
    
    (map-set asset-pools
      { pool-id: pool-id }
      (merge pool-info { is-active: false })
    )
    
    (ok true)
  )
)

;; Admin function to update platform fee
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-rate u1000) ERR-INVALID-AMOUNT) ;; Max 10%
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)
