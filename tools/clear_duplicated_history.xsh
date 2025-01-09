$[sqlite3 $XONSH_HISTORY_FILE @("DELETE FROM xonsh_history WHERE rowid NOT IN "
                                "(SELECT max(rowid) FROM xonsh_history GROUP BY inp);")]
