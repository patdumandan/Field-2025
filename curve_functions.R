n_draw_lines <- 100
set.seed(123)

summarise_curve = function(draw_matrix, temp_pred) {
  data.frame(
    temp = temp_pred,
    median = apply(draw_matrix,2,median),
    mean = apply(draw_matrix,2,mean),
    lower = apply(draw_matrix,2,quantile,probs = 0.025),
    upper = apply(draw_matrix,2,quantile,probs = 0.975))}

posterior_lines= function(draw_matrix,temp_pred,n_draws = 100) {
  n_draws <- min(n_draws,nrow(draw_matrix))
  selected <- sample(seq_len(nrow(draw_matrix)),n_draws)
  as.data.frame(draw_matrix[selected, , drop = FALSE])%>%
    mutate(draw = seq_len(n_draws))%>%
    pivot_longer(cols = -draw,names_to = "prediction",values_to = "value" )%>%
    group_by(draw)%>%mutate(temp = temp_pred) %>%ungroup()}
