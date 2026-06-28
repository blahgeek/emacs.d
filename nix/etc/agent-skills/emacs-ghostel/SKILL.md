---
name: emacs-ghostel
description: ghostel是运行在emacs中的terminal emulator（ghostty el），类似tmux，它支持使用命令发送输入并获取当前显示内容。当用户指定使用ghostel时，读取该技能。
---

当用户要求使用ghostel时，用户应该提供一个ghostel terminal的名称，这个名称由两个单词组成，比如"fair-table"。这个名称必须由用户提供，你无法获取所有正在运行的ghostel terminal的列表。

当拿到一个ghostel terminal的名称后，你可以向它发送输入来执行命令，以及获取当前的输出内容。

## 发送输入

这是类似 `tmux send-keys` 的功能。具体命令如下：

```bash
# 向"fair-table"这个终端输入ls命令并回车
echo '["ghostel-send-input", "fair-table", "ls", 13]' | emacs-safeclient

# 向"flag-ship"这个终端发送control-c
echo '["ghostel-send-input", "flag-ship", 3]' | emacs-safeclient

# 向"fair-table"这个终端输入一个hello，然后发送一个组合键 ctrl+shift+t
echo '["ghostel-send-input", "fair-table", "hello", ["key", "t", "shift,ctrl"]]' | emacs-safeclient
```

- echo的内容是一个json list。注意转义
- json第一个元素"ghostel-send-input"是命令名称，保持不变
- json第二个元素是ghostel terminal的名称，由用户提供
- json后面的所有元素都是输入的内容。输入的内容可以是：
  - 字符串：作为 raw text 直接发送，也支持ANSI转义序列（但需要注意bash和json的转义）。
  - 数字，表示ascii code，比如，3表示control-c；13表示return；127表示backspace
  - `["key", KEY]` 或 `["key", KEY, MODS]`：通过ghostel的key encoder发送按键。KEY可以是`"return"`、`"up"`、`"down"`、`"left"`、`"right"`、`"backspace"`、`"tab"`、`"escape"`、`"delete"`、`"home"`、`"end"`、`"f1"`等；MODS是可选的逗号分隔修饰键，比如`"ctrl"`、`"shift,ctrl"`、`"meta"`。

emacs-safeclient的输出正常情况是"null"。如果是其他输出则可能表示执行异常。

## 获取终端内容

这是类似 `tmux capture-pane` 的功能。具体命令如下：

```bash
# 获取"flag-ship"这个终端的*全部*内容
echo '["ghostel-get-content", "flag-ship"]' | emacs-safeclient

# 使用jq -r把返回的json string变成裸字符串，然后用tail截取
echo '["ghostel-get-content", "flag-ship"]' | emacs-safeclient | jq -r | tail -10

# 获取"fair-table"这个终端最后10行
echo '["ghostel-get-content", "fair-table", -10, 0]' | emacs-safeclient

# 获取从terminal最后往上数100行开始的10行内容
echo '["ghostel-get-content", "fair-table", -100, -90]' | emacs-safeclient
```

- echo的内容是一个json list。注意转义
- json第一个元素"ghostel-get-content"是命令名称，保持不变
- json第二个元素是ghostel terminal的名称，由用户提供
- json第三个元素（可选）是起始行号 `start`，从terminal内容末尾往前数，通常是负数。例如 `-10` 表示最后10行的起点。不指定时默认从最开始返回
- json第四个元素（可选）是结束行号 `end`，同样从terminal内容末尾往前数。`0` 表示末尾，`-90` 表示末尾往上数90行的位置。不指定时默认到终端最后
- emacs-safeclient的正常输出是一个带引号的json string，表示获取的内容。因此需要注意转义。你可以使用"jq -r"变成裸字符串。


## 注意事项

1. 你可能需要轮询terminal的输出来判断一个命令是否运行完成。对于没有明显结束输出的命令，建议在运行时加上相应的打印，比如运行 `run long-running-command; echo "=== long-running-command finished ==="，这样就可以通过输出明确判断命令是否运行完成。
