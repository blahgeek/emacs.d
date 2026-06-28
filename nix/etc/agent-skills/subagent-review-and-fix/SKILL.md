---
name: subagent-review-and-fix
description: 调用subagent对代码改动进行独立review，根据review意见修改完善。
disable-model-invocation: true
---

调用 `subagent` tool，指派一个subagent来完成独立的代码review，并按需修复。

用户可能会指定review的代码范围。比如如果指定了类似 `HEAD`, `HEAD^`, `abc1234..cde2345` ，那表示review相应的git commit(s)。如果用户没有指定，那么默认为最近完成修改的功能，可能是还未git commit的内容，或者最新一个commit。如果无法根据上下文明确判断，则向用户确认review范围。

## 步骤

1. 先把需要review的代码改动单独生成到一个或多个临时文件，放到/tmp下。可以通过`git diff`、`git format-patch`等命令。
   
2. 调用`subagent` tool进行review。具体见下

3. 对review的结果进行分析和后续操作。具体见下

## 调用subagent的参数

### readonly

用于代码review时，总是设置`readonly`为true

### cwd

指定被review的代码项目的根目录，以绝对路径指定

### prompt

调用subagent的prompt中总是应该以 `/skill:code-review ` 开头（表示让它指定代码审查的技能），后面跟上内容，包括：

1. 刚才生成的.diff或.patch文件路径
2. 改动的目的和原因。如果是通过`git format-patch`等方式生成的文件中已经包括了message的情况，则可以跳过。
3. 当前代码文件的状态（是否是修改后的版本）

prompt举例：

```
/skill:code-review 我实现了一版代码改动，功能是给foo加上bar的支持，同时保证baz不受影响；改动的diff位于 /tmp/input.xxxxx.diff 这个文件；当前项目目录中的代码是修改后的版本。请根据技能review这个改动
```

注意：
1. subagent没有权限调用git等命令，因此无法让他直接通过git命令获取需要review的改动。
2. **一定不要在prompt中包含比如“关注点”，“可能有问题的地方”等描述**，使得让subagent完成独立第三方评审

## subagent review完成之后的工作

分析和理解subagent输出的审查意见，逐条处理。

a. 如果审查指出了明确合理的bug或者很合理的建议，那么你应该直接进行修复
b. 如果你认为审查指出的建议不合理，或者与用户的需求和计划不匹配，或者可能带来比较大的重构，那么你应该与用户确认后再进行下一步操作
c. 最终你应该输出总结，告诉用户审查的意见以及修复情况
