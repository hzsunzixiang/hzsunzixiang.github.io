---
title: RabbitMQ 依赖模块源码分析
date: 2026-01-17
---

<a id="top"></a>

[← 返回 RabbitMQ](/rabbitmq/) | [← 返回首页](/)

# RabbitMQ 依赖模块源码分析

![RabbitMQ](/images/rabbitmq/rabbitmq_icon.png)

本专栏深入分析 RabbitMQ 的各个依赖模块源码，帮助理解 RabbitMQ 底层实现原理。

---

## 📚 依赖模块分析列表

### 系统集成

| 模块 | 文档 | 描述 |
|------|------|------|
| systemd | [完整指南](/2026/01/18/rabbitmq-rabbitmq-deps-systemd-guide/) | Linux 服务管理与 RabbitMQ 集成实践 |
| rabbitmq_prelaunch | [深度解析](/2026/01/18/rabbitmq-rabbitmq-prelaunch-analysis/) | 预启动模块：环境检查、配置加载、分布式初始化 |

### 编码与序列化

| 模块 | 文档 | 描述 |
|------|------|------|
| base64url | [深度解析](/2026/01/16/rabbitmq-rabbitmq-deps-base64url-analysis/) | URL 安全的 Base64 编解码器，RFC 4648 标准实现 |

### 网络与协议

| 模块 | 文档 | 描述 |
|------|------|------|
| *待添加* | - | - |

### 分布式与存储

| 模块 | 文档 | 描述 |
|------|------|------|
| *待添加* | - | - |

### 工具与辅助

| 模块 | 文档 | 描述 |
|------|------|------|
| *待添加* | - | - |

---

## 📊 依赖模块概览

RabbitMQ 4.0.5 共有 86 个依赖模块，按功能分类：

| 分类 | 数量 | 主要模块 |
|------|------|----------|
| 核心架构 | 3 | rabbit, rabbit_common, amqp_client |
| 分布式存储 | 3 | ra, osiris, khepri |
| 协议支持 | 4 | rabbitmq_mqtt, rabbitmq_stomp, rabbitmq_stream |
| 编码序列化 | 5+ | base64url, jsx, thoas, csv |
| 认证授权 | 6+ | OAuth2, LDAP, HTTP Auth |
| 监控管理 | 3+ | prometheus, management |

---

## 🔗 相关资源

- [RabbitMQ 依赖模块全面分析](/2026/01/17/rabbitmq-rabbitmq-dependencies-analysis/) - 86 个依赖模块的功能概述
- [RabbitMQ 技术文档](/rabbitmq/) - 完整的 RabbitMQ 技术文档集合
- [RabbitMQ GitHub 仓库](https://github.com/rabbitmq/rabbitmq-server)

---

[← 返回 RabbitMQ](/rabbitmq/) | [← 返回首页](/) | [↑ 返回顶部](#top)

*本专栏持续更新中。*
