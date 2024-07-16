getBudget <- function(){
  budgetQuery(
    paste(
      "SELECT bu.*, pd.position AS position, pd.class AS class, pd.tpe AS tpe, pd.name AS player, mbb.username AS username, o.name AS organization, t.name AS team 
      FROM budgetplayers bu 
      JOIN portaldb.organizations o ON bu.org = o.id 
      JOIN portaldb.teams t ON bu.org = t.orgID AND bu.affiliate = t.affiliate
      LEFT JOIN portaldb.playerdata pd ON bu.pid = pd.pid
      LEFT JOIN mybbdb.mybb_users mbb ON pd.uid = mbb.uid;"
    )
  ) %>% 
    future_promise()
}

getBudgetPlayer <- function(pid){
  budgetQuery(
    paste(
      "SELECT * FROM budgetplayers WHERE pid = ", pid, ";"
    )
  ) %>% 
    future_promise()
}