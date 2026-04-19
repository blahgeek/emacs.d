---
name: tavily
description: 使用 Tavily API 进行 Web 搜索、网页内容提取、站点爬取与站点地图生成。当用户需要联网搜索信息、抓取某个 URL 的正文内容、批量抓取某个站点的页面或生成站点链接地图时使用。获取网页内容需求优先使用此方式，失败后再尝试浏览器等其他方式。
---

# Tavily

Tavily 提供一套针对 LLM 优化的 Web 搜索与抓取 API。本 skill 通过 `tavily.sh` 这个 curl wrapper 调用各个 endpoint。

## 前置条件

- 环境变量 `TAVILY_API_KEY` 必须已设置。
- 通过同目录下的 `tavily.sh` 脚本调用：

```bash
tavily.sh <endpoint> '<json-body>'
```

`<endpoint>` 是 `search` / `extract` / `crawl` / `map` 之一。`<json-body>` 直接作为 HTTP body 透传给 `https://api.tavily.com/<endpoint>`，wrapper 不做任何额外处理，所以参数名与类型必须严格与下面的说明一致。

## 选用哪个 endpoint

- **search**：通用 Web 搜索，输入一个自然语言 query，返回相关网页结果（可选 LLM 摘要 answer）。适合"帮我搜一下 XXX"之类的需求。
- **extract**：给定一个或一组 URL，抓取并返回页面正文内容。适合"读取这个网页的内容"。
- **crawl**：从一个起点 URL 出发在站内爬取多个页面并返回其正文。适合"抓取这个文档站里所有 XX 相关的页面"。
- **map**：与 crawl 类似的站内遍历，但只返回 URL 列表，不抓取正文。适合"列出某站点下所有页面 URL"。

## search

对应 `POST /search`。

必需参数：

- `query` (string)：要搜索的查询。

常用可选参数：

- `search_depth` (`basic` | `advanced` | `fast` | `ultra-fast`，默认 `basic`)：`advanced` 相关性最高但更慢（2 credits/次），其它 1 credit/次。
- `max_results` (int, 0–20, 默认 5)：返回结果数量。
- `topic` (`general` | `news` | `finance`，默认 `general`)：`news` 适合实时新闻类查询。
- `time_range` (`day` | `week` | `month` | `year` 或 `d`/`w`/`m`/`y`)：按发布时间过滤。
- `start_date` / `end_date` (string, `YYYY-MM-DD`)：按日期范围过滤。
- `include_answer` (bool 或 `basic` / `advanced`，默认 false)：是否返回 LLM 生成的简短答案。
- `include_raw_content` (bool 或 `markdown` / `text`，默认 false)：是否把每个结果页的正文也返回。
- `include_domains` / `exclude_domains` (string[])：限定/排除结果来源域名。
- `country` (string)：结果按某国家加权（仅 `topic=general` 时可用）。

示例：

```bash
tavily.sh search '{"query":"Who is Leo Messi?","include_answer":true,"max_results":5}'
```

返回 JSON 主要字段：

- `query` (string)：原始 query。
- `answer` (string | null)：LLM 摘要，仅在请求 `include_answer` 时出现。
- `results` (array)：每项 `{ title, url, content, score, raw_content }`。`content` 是针对 query 的相关片段/摘要，`raw_content` 仅在 `include_raw_content` 时非空。
- `images` (array)：仅在 `include_images=true` 时非空。

## extract

对应 `POST /extract`。

必需参数：

- `urls` (string 或 string[])：要抓取正文的 URL；单条或数组均可。

常用可选参数：

- `extract_depth` (`basic` | `advanced`，默认 `basic`)：`advanced` 能抓到表格/嵌入内容，但更慢（2 credits/5 条成功 vs 1 credit/5 条成功）。
- `format` (`markdown` | `text`，默认 `markdown`)：正文输出格式。
- `query` (string)：若提供，会按此 query 对正文 chunks 做重排，突出相关片段。
- `chunks_per_source` (int, 1–5, 默认 3)：仅在提供 `query` 时有效，控制每个 URL 返回的片段数。
- `include_images` (bool, 默认 false)：是否返回页面中的图片列表。
- `timeout` (float, 1–60 秒)：单次抓取的超时。

示例：

```bash
tavily.sh extract '{"urls":"https://en.wikipedia.org/wiki/Artificial_intelligence","format":"markdown"}'
```

```bash
tavily.sh extract '{"urls":["https://example.com/a","https://example.com/b"],"extract_depth":"advanced"}'
```

返回 JSON 主要字段：

- `results` (array)：抓取成功的页面，每项 `{ url, raw_content, images }`。`raw_content` 是整页正文（markdown 或 text，取决于 `format`）。
- `failed_results` (array)：抓取失败的 URL 列表，每项含 `url` 与 `error`。

## crawl

对应 `POST /crawl`。从一个起点 URL 出发，按图结构遍历站内链接并抓取正文。

必需参数：

- `url` (string)：起点 URL。

常用可选参数：

- `instructions` (string)：自然语言指令，约束爬虫关注什么内容（启用后成本变为 2 credits/10 页）。
- `max_depth` (int, 1–5, 默认 1)：从起点出发的最大跳数。
- `max_breadth` (int, 1–500, 默认 20)：每一层最多跟进多少条链接。
- `limit` (int, 默认 50)：整个任务最多处理多少条链接。
- `select_paths` / `exclude_paths` (string[])：用正则过滤路径，如 `"^/docs/.*"`。
- `select_domains` / `exclude_domains` (string[])：用正则过滤域名。
- `allow_external` (bool, 默认 true)：是否允许跨域。
- `extract_depth` (`basic` | `advanced`，默认 `basic`)：同 extract。
- `format` (`markdown` | `text`, 默认 `markdown`)。
- `timeout` (float, 10–150 秒, 默认 150)。

示例：

```bash
tavily.sh crawl '{"url":"docs.tavily.com","instructions":"Find all pages about the Python SDK","max_depth":2,"limit":30}'
```

返回 JSON 主要字段：

- `base_url` (string)：起点 URL。
- `results` (array)：每项 `{ url, raw_content }`，覆盖遍历到的所有页面。

## map

对应 `POST /map`。与 crawl 参数基本一致，但只返回 URL 列表，不抓正文，因此没有 `extract_depth` / `format` 等和抓取相关的参数。

必需参数：

- `url` (string)：起点 URL。

常用可选参数：`instructions`、`max_depth` (1–5, 默认 1)、`max_breadth` (1–500, 默认 20)、`limit` (默认 50)、`select_paths` / `exclude_paths`、`select_domains` / `exclude_domains`、`allow_external` (默认 true)、`timeout` (10–150 秒, 默认 150)。

示例：

```bash
tavily.sh map '{"url":"docs.tavily.com","max_depth":2}'
```

返回 JSON 主要字段：

- `base_url` (string)：起点 URL。
- `results` (string[])：遍历到的 URL 列表（不含正文）。

## 使用建议

- 只是想"搜一下某个话题"：用 `search`，通常带上 `"include_answer":true` 可以直接拿到总结。
- 已经知道要读哪个 URL：用 `extract`，比 `search` 更省、正文更完整。
- 需要从一个文档站批量读多个页面：先用 `map` 列出 URL，再挑选出需要的传给 `extract`；或者直接 `crawl` 一步到位。
- 结果是 JSON，可按需用 `jq` 做后续处理，例如：

```bash
tavily.sh search '{"query":"tavily api"}' | jq -r '.results[].url'
```
