---
name: jujutsu-resolve-conflict
description: 当解决jujutsu repo代码冲突时阅读此技能。当发现.jjconflict*文件时说明是jujutsu repo。
---

jujutsu (jj) 是兼容git的版本管理工具。总体来说，按照标准正常的git conflict代码冲突解决流程进行。整个过程只使用 git 命令，不使用任何 jj 命令（你很可能会被拒绝运行jj命令，因为.jj目录权限限制）；你可以假装jujutsu不存在。

额外需要注意的点：

1. **忽略所有 `.jjconflict*` 的文件和目录**。jujutsu 会额外生成一些专有的文件，你无须关心。总是使用 `git status ':!/.jjconflict*' ':!/JJ-CONFLICT-README'` 命令来查看当前状态。

2. conflict marker中可能的额外格式

生成的conflict marker可能长这样：

    <<<<<<< yytoyxsl 4a92b5bd "commit message 1..."
    ||||||| uotrzlws 4011d0cd "commit message 2..."
    =======
    >>>>>>> kmvxrlyk 312fc248 "commit message 3..."
    
其中 yytoyxsl, uotrzlws, kmvxrlyk这种不是十六进制字符的请忽略；后面的4a92b5bd，4011d0cd，312fc248全部是十六进制字符的才是git commit id。(git commit id可以在需要的时候用来查看改动来自的commit的描述和历史。)

**如果是用户单独要求的解决冲突任务**：

1. **编辑之后不要git commit或git rebase --continue等**。你只需要完成代码的改动，最终确认所有冲突都已解决即可。不要运行git命令来提交解决后的版本。

2. 完成之后，请总结主要的冲突点、解决方式、以及可能需要review和关注的位置。
