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

getOrgTransactions <- function(){
  budgetQuery(
    paste(
      "SELECT 
          t.tid,
          t.link,
          t.type,
          t.transfervalue,
          p.name AS name_player,
          tp.toOrg AS toOrg_player,
          org_pl_to.abbr AS orgName_player,
          CONCAT('S', dp.season, ' R', dp.round, ' ', org_orig.abbr) AS name_pick,
          tpk.toOrg AS toOrg_pick,
          org_pi_to.abbr AS orgName_pick
      FROM 
          budgetdb.transactions t
      LEFT JOIN 
          budgetdb.transactionsplayers tp ON t.tid = tp.tid
      LEFT JOIN 
          portaldb.playerdata p ON tp.pid = p.pid
      LEFT JOIN 
          portaldb.organizations org_pl_to ON tp.toOrg = org_pl_to.id
      LEFT JOIN 
          budgetdb.transactionspicks tpk ON t.tid = tpk.tid
      LEFT JOIN
      	portaldb.organizations org_pi_to ON tpk.toOrg = org_pi_to.id
      LEFT JOIN 
          budgetdb.draftpicks dp ON tpk.pickid = dp.pickid
      LEFT JOIN 
          portaldb.organizations org_orig ON dp.original = org_orig.id
      LEFT JOIN 
          portaldb.organizations org_curr ON dp.current = org_curr.id;"
    )
  ) %>% 
    future_promise()
}