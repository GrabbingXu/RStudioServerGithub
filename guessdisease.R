library(httr)
library(jsonlite)
library(rvest)

# 初始化全局配置
init_session <- function() {
  handle_reset("https://xiaoce.fun")  # 重置会话
  
  # 获取初始页面的CSRF token
  initial_page <- GET("https://xiaoce.fun/guessdisease")
  csrf_token <- initial_page %>%
    content("text") %>%
    str_extract('name="csrf-token" content="([^"]+)"', group = 1)
  
  # 从localStorage获取认证token（需手动获取）
  auth_token <- "YOUR_LOCALSTORAGE_TOKEN"  # 通过浏览器控制台获取
  
  list(
    handle = handle("https://xiaoce.fun"),
    headers = add_headers(
      "Authorization" = paste("Bearer", auth_token),
      "X-CSRF-TOKEN" = csrf_token,
      "Referer" = "https://xiaoce.fun/guessdisease",
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
    )
  )
}

# 发送对话消息
send_message <- function(session_config, message) {
  body <- list(
    message = message,
    session_id = Sys.getenv("SESSION_ID", unset = generate_session_id()),
    timestamp = as.integer(Sys.time())
  )
  
  response <- POST(
    handle = session_config$handle,
    path = "/api/chat",
    config = session_config$headers,
    body = toJSON(body, auto_unbox = TRUE),
    content_type_json()
  )
  
  if(status_code(response) == 200) {
    content(response)$reply
  } else {
    stop(sprintf("请求失败，状态码：%d", status_code(response)))
  }
}

# 生成会话ID（逆向自前端实现）
generate_session_id <- function() {
  timestamp <- as.character(as.integer(Sys.time() * 1000))
  paste0(
    sample(letters, 8, replace = TRUE) %>% paste(collapse = ""),
    "-",
    digest::digest(timestamp, algo = "md5") %>% substr(1, 12)
  )
}

# 使用示例
session <- init_session()
reply <- send_message(session, "您好，医生。")
cat("Bot回复:", reply)