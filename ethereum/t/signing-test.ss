(export #t)

(import
  :gerbil/gambit/exceptions
  :std/format :std/sugar
  :std/test
  :clan/poo/poo :clan/poo/brace
  ../hex ../types ../ethereum ../known-addresses ../signing)

(def (kp x) (keypair<-secret-key (bytes<-0x x) ""))

(def trent-keys (kp "0xb6fb0b7e61363ee2f748161338f56953e8aa42642e9990eff17e7de9aa895786"))
(def trent-address (keypair-address trent-keys))
(def alice-keys (kp "0xfdc8f15b2dd9229b0b9246094393afc23b3b705c07e674f6cb614120d1627818"))
(def alice-address (keypair-address alice-keys))
(def bob-keys (kp "0x9b21b9b06ba77824b8ba6a815f5a075229a708ae88ba7fd935c968fe2c3df172"))
(def bob-address (keypair-address bob-keys))
(def yolanda-keys (kp "0xaedcdea2b91de24d1fe2c8ae4b60687fb3826612962553fa3d0b8486e322aaa7"))
(def yolanda-address (keypair-address yolanda-keys))
(def zander-keys (kp "0x4884b1bdef8281b40cad15f5525d72a5c9a5db18f213abf28a46bfab8bff2a5f"))
(def zander-address (keypair-address zander-keys))

(def test-keypairs
  [["Alice" alice-keys]
   ["Trent" trent-keys]
   [ "Bob" bob-keys]])

(def (register-test-keypairs)
  (for-each (cut apply register-keypair <>) test-keypairs))

(def signing-test
  (test-suite "Test suite for glow/ethereum/signing"
    (register-test-keypairs)
    (test-case "check test users"
      (defrule (check-user name keys address pubkey)
        (let* ((data (format "some arbitrary string for ~a to sign" name))
               (signature (make-signature String (keypair-secret-key keys) data)))
          (check-equal? (json<- PublicKey (keypair-public-key keys)) pubkey)
          (check-equal? (json<- Address (keypair-address keys)) address)
          (check-equal? (signature-valid? String (keypair-address keys) signature data) #t)))
      (check-user "Alice" alice-keys "0xC54e86DFFb87B9736E2E35DD85c775358F1c31CE"
                  "0x045562695c85f88f6cbaec121d2a3da6666c5dc8540d86358bd569a1882bbe6ddcf45b76f5643133939c8e7a339947ca1b115290d577343023d79c256dbc54bc97")
      (check-user "Bob" bob-keys "0x9CcaEd210CE8c0Cb49c5Ad1C4f583406c264BA69"
                  "0x049e0a7e3c05e3328c603b0c27fbfdfc5030c95d9ad179a431c14f81e30a64ce95f625447e182a8be718d45f9ab9723f9b8571dd5c5752daa66feb84938b095805")
      (check-user "Trent" trent-keys "0xF47408143d327e4bc6A87EF4a70A4E0aF09b9A1C"
                  "0x0426bd9885f2c9e23d18c3025da70e71a4f7ce237124352882eafbd1cbb1e9742c4fe3847ce1a56a0d19df7a7d385a2134be05208b5d1ccc5d015f5e9a3ba0d7df"))))
