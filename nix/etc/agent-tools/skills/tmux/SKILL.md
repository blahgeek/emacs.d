---
name: tmux
description: 如果需要运行可交互程序（比如gdb），或者需要ssh到其他机器，或者用户指定使用tmux，请务必根据这里的指引使用tmux完成。
---

使用tmux的几种情况：

1. 当需要运行可交互程序（比如gdb）时，可以直接启动一个新的 tmux session
(a) 你启动 session 时应遵循一定的命名规范，格式为：AGENT_NAME-当前任务名-随机数（注意不能有空格）
(b) 创建之后，你总是应该告知用户你创建了该 session，并提供完整的 attach 命令

2. 当你需要ssh到其他机器时，你必须请求用户提供一个创建好的tmux session并在里面由用户在里面建立好ssh连接，然后你来直接使用。你如果直接运行ssh会没有权限。

3. 其他用户指定使用tmux的情况。


无论哪种情况，在获得 tmux session 后，你都应该通过 tmux 命令向其发送输入以及获取当前panel的内容。
Tmux 中当前 panel 的内容可能很长，可以考虑结合 tail 或者 head 等命令去获取。

请记住，用户可能会操作该 session，因此务必在每次操作中都确认并选择当前选中的 window 和 panel。你可以自己创建新的 window 和 panel，并建议给它们起有意义的名字。

请务必只能连接你自己创建的 session 或者用户给定的 session，不能去任意连接无关的 session。


## 常用命令速查

### 查找 Tmux Session

**列出所有 session：**
```bash
tmux list-sessions
```

**列出某 session 的所有 window：**
```bash
tmux list-windows -t <session-name-or-id>
# 示例：
tmux list-windows -t 0
```

输出示例：`0: ssh* (1 panes) [104x23] @0 (active)`，其中 `*` 表示当前激活的 window。

---

### 发送输入到 Pane

```bash
tmux send-keys -t <session>:<window>.<pane> "<命令>" Enter
```

- `Enter` 是关键字，代表回车键
- pane 部分可省略（默认 pane 0）：`-t 0:0` 等价于 `-t 0:0.0`
- 发送后需等待命令执行，再获取输出：

```bash
tmux send-keys -t 0:0 "ip addr show" Enter && sleep 2 && tmux capture-pane -t 0:0 -p
```

---

### 获取 Pane 内容

**获取当前可见内容：**
```bash
tmux capture-pane -t <session>:<window>.<pane> -p
# 示例：
tmux capture-pane -t 0:0 -p
```

**只看最后 N 行（输出较长时）：**
```bash
tmux capture-pane -t 0:0 -p | tail -20
```

**向上捕获历史滚动区域：**
```bash
tmux capture-pane -t 0:0 -p -S -500   # 往上捕获 500 行
```

---

### 创建新 Session 并运行程序

随机数必须在 shell 命令执行时生成（用 `$RANDOM`），不能由 Claude 自己写死一个数字：

```bash
SESSION="claude-任务名-$RANDOM" && tmux new-session -d -s "$SESSION" && tmux send-keys -t "$SESSION" "ping google.com" Enter && echo "Session: $SESSION"
```

- `-d` 表示后台创建（不自动 attach）
- 用变量 `$SESSION` 贯穿后续所有命令，避免名字不一致
- 命令末尾 `echo` 输出实际 session 名，方便告知用户 attach 命令

创建后务必告知用户：
```bash
tmux attach -t claude-任务名-16546
```

---

### 完整操作流程示例（SSH 场景）

```bash
# 1. 确认 session 存在
tmux list-sessions

# 2. 查看 window 列表，确认当前激活 window
tmux list-windows -t 0

# 3. 查看 pane 当前状态（确认已 SSH 连接、shell prompt 等）
tmux capture-pane -t 0:0 -p

# 4. 发送命令并获取输出
tmux send-keys -t 0:0 "ip addr show" Enter && sleep 2 && tmux capture-pane -t 0:0 -p

# 5. 继续执行下一条命令
tmux send-keys -t 0:0 "ip route show" Enter && sleep 1 && tmux capture-pane -t 0:0 -p
```
