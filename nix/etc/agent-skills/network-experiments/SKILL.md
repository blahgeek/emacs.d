---
name: network-experiments
description: 做需要 NET_ADMIN 权限的网络实验时使用：在 sandbox（user namespace）内创建独立的 network namespace，用 slirp4netns 提供用户态网络出口，配置 resolv.conf，并在 namespace 内使用 iptables/nftables/tc/iproute2 等工具做实验。适用于"创建 netns 做实验""在隔离网络环境里测试 curl/防火墙规则"等需求。
---

# network-experiments

当前环境是带 user namespace 的 sandbox，进程是 ns 内的 root，对自己创建的 network namespace 拥有完整 `CAP_NET_ADMIN`，可以自由做网络实验，而**不影响宿主网络**。

**注意：下面提到的 netns-exp.sh 位于当前目录下，运行时请替换成绝对路径 `${current_skill_dir}/netns-exp.sh`**

## 快速用法（推荐）

```bash
# 1. 创建实验环境：新 netns + slirp4netns(tap0) + resolv.conf
${current_skill_dir}/netns-exp.sh setup
#    输出 holder PID（记为 $PID）和状态目录 /tmp/netns-exp.$PID

# 2. 在 netns 内执行任意命令（自动 nsenter -n -m）
${current_skill_dir}/netns-exp.sh run $PID curl -sS https://baidu.com -o /dev/null -w '%{http_code}\n'
${current_skill_dir}/netns-exp.sh run $PID iptables -A OUTPUT -p tcp --dport 443 -j REJECT
${current_skill_dir}/netns-exp.sh run $PID iptables -L -n -v

# 3. 实验结束必须清理（kill slirp 与 holder 进程）
${current_skill_dir}/netns-exp.sh teardown $PID
```

也可以不用 `run`，直接 `nsenter -t $PID -n -m CMD...`，效果相同。

## 原理与手动步骤（脚本内部做的事，需要定制时参考）

1. **创建 netns + 私有 mount ns**：
   ```bash
   unshare --net --mount sleep 86400 &
   PID=$!
   ```
   mount ns 用于后面 bind-mount resolv.conf，避免污染外部环境的 `/etc/resolv.conf`（`unshare --mount` 默认 propagation private）。

2. **启动 slirp4netns** 给该 netns 提供用户态网络出口：
   ```bash
   slirp4netns --configure --mtu=1500 --enable-ipv6 $PID tap0 &
   ```
   - 找不到 `slirp4netns` 时（不在 PATH 中），先确认是否已全局安装，再重试。
   - `--configure` 自动配好 tap0：IPv4 `10.0.2.100/24`，默认路由 via `10.0.2.2`。
   - 网关 `10.0.2.2`、DNS 转发器 `10.0.2.3` 由 slirp 内置（可 ping 通 10.0.2.2 验证）。

3. **配置 resolv.conf**（slirp4netns 不会代劳）：
   ```bash
   printf 'nameserver 10.0.2.3\noptions ndots:0\n' > /tmp/resolv.conf.netns
   nsenter -t $PID -m mount --bind /tmp/resolv.conf.netns /etc/resolv.conf
   ```

4. 在 netns 内做实验：`nsenter -t $PID -n -m CMD...`。iptables（nf_tables 后端）、nft、tc 等均可正常使用。

5. 清理：kill slirp4netns 与 holder 进程，删除临时文件。

## 已验证的坑（重要）

1. **必须加 `--enable-ipv6`**。本机上游网络对很多站点只能通过 IPv6 出站（例如 `google.com` 的 DNS 只返回 AAAA 记录）。不开 IPv6 时，netns 内 `curl google.com` 会在 connect 阶段失败；开启后 slirp 会通过 SLAAC 配出 `fd00::/64` 地址和 `default via fe80::2` 路由。
2. **DNS 不自动可用**：不配置 resolv.conf 指向 `10.0.2.3` 时，netns 内域名解析会沿用外部配置而失败。
3. slirp 的转发走的是 sandbox 自身的网络栈（在外层 netns 里建 socket），所以外部网络本身必须可用；宿主禁用的目标在 netns 内同样不通。
4. 实验做完**务必 teardown**，否则 holder/slirp 进程会一直残留。

## 验证清单（建好环境后自检）

```bash
${current_skill_dir}/netns-exp.sh run $PID ping -c 2 -W 2 10.0.2.2        # slirp 网关通
${current_skill_dir}/netns-exp.sh run $PID curl -sS -o /dev/null -w '%{http_code}\n' https://google.com   # IPv6 出站
${current_skill_dir}/netns-exp.sh run $PID curl -sS -o /dev/null -w '%{http_code}\n' https://baidu.com    # IPv4 出站
${current_skill_dir}/netns-exp.sh run $PID iptables -L -n -v              # netfilter 可用
```
