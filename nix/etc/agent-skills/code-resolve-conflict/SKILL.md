---
name: code-resolve-conflict
description: 当解决代码冲突时（比如来自git rebase或者git merge）阅读此技能
---

总体来说，按照标准正常的git conflict代码冲突解决流程进行。

额外需要注意的点：

1. **忽略所有 `.jjconflict*` 的文件和目录**。用户可能使用jujutsu (jj)工具，会额外生成一些专有的文件，你无须关心。总是使用 `git status ':!/.jjconflict*' ':!/JJ-CONFLICT-README'` 命令来查看当前状态。

2. conflict marker中可能的额外格式

用户生成的conflict marker可能长这样：

    <<<<<<< yytoyxsl 4a92b5bd "commit message 1..."
    ||||||| uotrzlws 4011d0cd "commit message 2..."
    =======
    >>>>>>> kmvxrlyk 312fc248 "commit message 3..."
    
其中 yytoyxsl, uotrzlws, kmvxrlyk这种不是十六进制字符的请忽略；后面的4a92b5bd，4011d0cd，312fc248全部是十六进制字符的才是git commit id。(git commit id可以在需要的时候用来查看改动来自的commit的描述和历史。)

3. **编辑之后不要git commit或git rebase --continue等**。你只需要完成代码的改动，最终确认所有冲突都已解决即可。不要运行git命令来提交解决后的版本。

4. 完成之后，请总结主要的冲突点、解决方式、以及可能需要review和关注的位置。
