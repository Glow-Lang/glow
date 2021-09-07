(hash (begin
        (ci 'begin
            (@list)
            (@list)
            (@list (ti 'begin 'end #f (@list) (@list '(payForSignature . #f)) (@list) (@list)))))
      (begin0 (ci 'begin0
                  (@list)
                  (@list)
                  (@list (ti 'begin0 'cp #f (@list) (@list) (@list) (@list)))))
      (cp (ci 'cp
              (@list)
              (@list (ti 'begin0 'cp #f (@list) (@list) (@list) (@list)))
              (@list (ti 'cp
                         'cp0
                         'Buyer
                         (@list (syntax (deposit! Buyer (@record (DefaultToken price)))))
                         (@list)
                         (@list '(Buyer . #f) '(price . #f))
                         (@list)))))
      (cp0 (ci 'cp0
               (@list)
               (@list (ti 'cp
                          'cp0
                          'Buyer
                          (@list (syntax (deposit! Buyer (@record (DefaultToken price)))))
                          (@list)
                          (@list '(Buyer . #f) '(price . #f))
                          (@list)))
               (@list (ti 'cp0
                          'end0
                          'Seller
                          (@list (syntax (withdraw! Seller (@record (DefaultToken price))))
                                 (syntax (publish! Seller signature)))
                          (@list '(tmp . #f) '(signature . #f))
                          (@list '(Seller . #f)
                                 '(price . #f)
                                 '(tmp . #f)
                                 '(isValidSignature . #f)
                                 '(Seller . #f)
                                 '(digest0 . #f)
                                 '(signature . #f)
                                 '(signature . Seller)
                                 '(Seller . #f))
                          (@list)))))
      (end (ci 'end
               (@list)
               (@list (ti 'begin 'end #f (@list) (@list '(payForSignature . #f)) (@list) (@list)))
               (@list)))
      (end0 (ci 'end0
                (@list)
                (@list (ti 'cp0
                           'end0
                           'Seller
                           (@list (syntax (withdraw! Seller (@record (DefaultToken price))))
                                  (syntax (publish! Seller signature)))
                           (@list '(tmp . #f) '(signature . #f))
                           (@list '(Seller . #f)
                                  '(price . #f)
                                  '(tmp . #f)
                                  '(isValidSignature . #f)
                                  '(Seller . #f)
                                  '(digest0 . #f)
                                  '(signature . #f)
                                  '(signature . Seller)
                                  '(Seller . #f))
                           (@list)))
                (@list))))
