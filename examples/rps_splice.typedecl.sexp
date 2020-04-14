(val inputHand : (-> (∩ 'tag1 bytes) Hand0))
(type Outcome = Outcome0)
(val inputOutcome : (-> (∩ 'tag2 bytes) Outcome0))
(type Hand = Hand0)
(val NatToHand : (-> int Hand0))
(constructor B_Wins : Outcome0)
(val NatToOutcome : (-> int Outcome0))
(val OutcomeToNat : (-> Outcome0 nat))
(constructor Draw : Outcome0)
(val HandToNat : (-> Hand0 nat))
(constructor Scissors : Hand0)
(constructor Rock : Hand0)
(val rockPaperScissors
     :
     (-> Participant
         Participant
         (∩ 'wagerAmount0
            'wagerAmount1
            'wagerAmount2
            'wagerAmount3
            'wagerAmount4
            'wagerAmount5
            int)
         Outcome0))
(constructor Paper : Hand0)
(val winner : (-> Hand0 Hand0 Outcome0))
(constructor A_Wins : Outcome0)
