#Global variable data frames for filtering (initial api calls)
df_arlis <- return_df_arlis()
dfs_by_year <- return_dflist_year()
df_total <- merge_dfs(dfs_by_year)
df_recents <- return_df_recents()