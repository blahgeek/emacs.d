---
name: emacs-eat
description: eat是运行在emacs中的terminal emulator，类似tmux，它支持使用命令发送输入并获取当前显示内容。当用户指定使用eat时，读取该技能。
---

当用户要求使用eat时，用户应该提供一个eat terminal的名称，这个名称由两个单词组成，比如"fair-table"。这个名称必须由用户提供，你无法获取所有正在运行的eat terminal的列表。

当拿到一个eat terminal的名称后，你可以向它发送输入来执行命令，以及获取当前的输出内容。

## 发送输入

这是类似 `tmux send-keys` 的功能。具体命令如下：

```bash
# 向"fair-table"这个终端输入ls命令并回车
echo '["eat-send-input", "fair-table", "ls", 13]' | emacs-safeclient

# 向"flag-ship"这个终端发送control-c
echo '["eat-send-input", "flag-ship", 3]' | emacs-safeclient
```

- echo的内容是一个json list。注意转义
- json第一个元素"eat-send-input"是命令名称，保持不变
- json第二个元素是eat terminal的名称，由用户提供
- json后面的所有元素都是输入的内容。输入的内容可以是一个字符串，也可以是一个数字。如果是数字的话，表示ascii code，比如，3表示control-c；13表示return；127表示backspace。如果是字符串的话，就是表示输入的内容，也支持ANSI转义序列（但需要注意bash和json的转义）。

emacs-safeclient的输出正常情况是"null"。如果是其他输出则可能表示执行异常。

## 获取终端内容

这是类似 `tmux capture-pane` 的功能。具体命令如下：

```bash
# 获取"flag-ship"这个终端当前显示的内容（不包括scrollback buffer）
echo '["eat-get-content", "flag-ship"]' | emacs-safeclient

# 使用jq -r把返回的json string变成裸字符串
echo '["eat-get-content", "flag-ship"]' | emacs-safeclient | jq -r

# 获取"fair-table"这个终端最多10行的scrollback buffer（历史） 以及 当前显示的内容
echo '["eat-get-content", "fair-table", 10]' | emacs-safeclient
```

- echo的内容是一个json list。注意转义
- json第一个元素"eat-get-content"是命令名称，保持不变
- json第二个元素是eat terminal的名称，由用户提供
- json第三个元素（可选）表示同时包含前面N行的历史。不指定时只返回当前屏幕显示的内容
- emacs-safeclient的正常输出是一个带引号的json string，表示获取的内容。因此需要注意转义。你可以使用"jq -r"变成裸字符串。


## 注意事项

1. 发送输入之前，请总是获取最后几行的内容，保证当前是可输入状态。用户也可能会同时操作给定的terminal，因此不能假定状态不变。
2. 你可能需要不断轮询terminal的输出来判断一个命令是否运行完成。对于没有明显结束输出的命令，建议在运行时加上相应的打印，比如运行 `run long-running-command; echo "=== long-running-command finished ==="，这样就可以通过输出明确判断命令是否运行完成。
