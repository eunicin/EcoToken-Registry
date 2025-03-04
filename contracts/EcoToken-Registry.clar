;; ******************************************************************
;; EcoToken: Verified Carbon Offset Registry
;; ******************************************************************
;; This registry implements environmental asset tokenization using Clarity.
;; It enables creation, trading, retirement, and verification of 
;; eco-credits as non-fungible tokens with provenance tracking.
;;
;; Core Capabilities:
;; - Issuance: Authorized issuance of individual or grouped environmental assets
;; - Provenance: Links each asset to verifiable data through URI references
;; - Custody: Secure tracking of asset custody throughout lifecycle
;; - Retirement: Permanent retirement mechanism for claimed offsets
;; - Verification: Ensures data integrity with format validation and batch controls
;;
;; Security Measures:
;; - Admin-gated issuance to maintain market integrity
;; - Retirement safeguards preventing reuse of claimed offsets
;;
;; Primary Application:
;; - Developed for voluntary carbon market registries
;; - Enables transparent tracking of environmental impact claims
;; ******************************************************************

(define-constant admin-principal tx-sender) ;; Registry administrator
(define-constant error-unauthorized (err u200)) ;; Error when non-admin attempts restricted operation
(define-constant error-not-asset-holder (err u201)) ;; Error when operation attempted by non-holder
(define-constant error-duplicate-asset (err u202)) ;; Error when asset already registered
(define-constant error-missing-asset (err u203)) ;; Error when asset doesn't exist
(define-constant error-invalid-asset-data (err u204)) ;; Error for malformed asset data
(define-constant error-retirement-failed (err u205)) ;; Error when retirement process fails
(define-constant error-retirement-unauthorized (err u206)) ;; Error for unauthorized retirement
(define-constant error-group-size-invalid (err u207)) ;; Error for invalid group size
(define-constant max-group-issuance u50) ;; Maximum eco-credits in single group issuance

;; ******************************************************************
;; Registry Storage
;; ******************************************************************
(define-non-fungible-token eco-asset uint) ;; Define eco-asset NFT type
(define-data-var registry-counter uint u0) ;; Tracks the latest registered asset ID

;; ******************************************************************
;; Data Mappings
;; ******************************************************************
(define-map asset-verification-data uint (string-ascii 256)) ;; Maps asset IDs to verification data
(define-map retired-assets uint bool) ;; Registry of permanently retired assets
(define-map asset-group-data uint (string-ascii 256)) ;; Stores metadata for asset groups

;; ******************************************************************
;; Internal Helper Functions
;; ******************************************************************

;; Verify caller is the current holder of specified asset
(define-private (is-asset-holder (asset-id uint) (caller principal))
  (is-eq caller (unwrap! (nft-get-owner? eco-asset asset-id) false)))


;; Validate verification data format and length
(define-private (is-valid-verification-data (data (string-ascii 256)))
  (let ((data-length (len data)))
    (and (>= data-length u1)
         (<= data-length u256))))

;; Check if asset has been permanently retired
(define-private (is-asset-retired (asset-id uint))
  (default-to false (map-get? retired-assets asset-id)))

;; Register single environmental asset
(define-private (register-single-asset (verification-data (string-ascii 256)))
  (let ((asset-id (+ (var-get registry-counter) u1)))
    (asserts! (is-valid-verification-data verification-data) error-invalid-asset-data)
    (try! (nft-mint? eco-asset asset-id tx-sender))
    (map-set asset-verification-data asset-id verification-data)
    (var-set registry-counter asset-id)
    (ok asset-id)))

;; Register single asset during group issuance
(define-private (register-single-in-group (data (string-ascii 256)) (previous-results (list 50 uint)))
  (match (register-single-asset data)
    success (unwrap-panic (as-max-len? (append previous-results success) u50))
    error previous-results))

;; Create number sequence for bulk operations
(define-private (create-number-sequence (count uint))
  (map - (list count)))

;; ******************************************************************
;; Public Registry Functions  
;; ******************************************************************

;; Register single environmental asset
(define-public (register-eco-asset (verification-data (string-ascii 256)))
    (begin
        ;; Verify caller has administrative privileges
        (asserts! (is-eq tx-sender admin-principal) error-unauthorized)

        ;; Explicitly validate verification data format
        (asserts! (is-valid-verification-data verification-data) error-invalid-asset-data)

        ;; Register the environmental asset
        (register-single-asset verification-data)))

;; Group registration of environmental assets
(define-public (group-register-eco-assets (data-list (list 50 (string-ascii 256))))
  (let ((group-size (len data-list)))
    (begin
      (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
      (asserts! (<= group-size max-group-issuance) error-group-size-invalid)
      (asserts! (> group-size u0) error-group-size-invalid)
      (ok (fold register-single-in-group data-list (list))))))

;; Retire environmental asset (permanent)
(define-public (retire-eco-asset (asset-id uint))
  (let ((asset-holder (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)))
    (asserts! (is-eq tx-sender asset-holder) error-not-asset-holder)
    (asserts! (not (is-asset-retired asset-id)) error-retirement-failed)
    (try! (nft-burn? eco-asset asset-id asset-holder))
    (map-set retired-assets asset-id true)
    (ok true)))

;; Retrieve current holder of asset
(define-read-only (get-asset-holder (asset-id uint))
  (ok (nft-get-owner? eco-asset asset-id)))

;; Retrieve latest registered asset ID
(define-read-only (get-latest-asset-id)
  (ok (var-get registry-counter)))

;; Check if asset is retired
(define-read-only (get-asset-retirement-status (asset-id uint))
  (ok (is-asset-retired asset-id)))

;; Retrieve group data for specific asset ID
(define-read-only (get-asset-group-data (asset-id uint))
  (ok (map-get? asset-group-data asset-id)))

;; Retrieve detailed metadata for specific asset
(define-read-only (get-asset-metadata (asset-id uint))
  (ok (map-get? asset-group-data asset-id)))

;; Verify caller is registry administrator
(define-read-only (is-caller-admin)
  (ok (is-eq tx-sender admin-principal)))
