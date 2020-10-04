@token
Scenario: Basic
  Given gnus start
  When I scan news
  Then I should see "evilhag"
  Then I should see "chelseaperetti"

@last_unread
Scenario: Updates newsrc with last unread id
  Given I should be in buffer "*Group*"
  And I go to word "evilhag"
  And I press "RET"
  Then I should be in buffer "*Summary nntwitter:evilhag*"
  And I press "C-n"
  And I press "C-n"
  And I press "RET"
  And I press "q"
  Then I should be in buffer "*Group*"
  Then I should see "99: nntwitter:evilhag"
  And I press "s"
  And I dump buffer
  Then newsrc's last unread id for "nntwitter:evilhag" is "1314403243007463424"
