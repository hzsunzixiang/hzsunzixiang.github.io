---
title: RabbitMQ 服务器编译指南
date: 2026-01-16 14:00:00
tags:
  - RabbitMQ
  - Erlang
  - 编译
  - macOS
categories:
  - 技术
  - RabbitMQ
---

# RabbitMQ 服务器编译指南

本文档记录了在 macOS (Apple Silicon) 环境下编译 RabbitMQ 服务器的完整过程，包括遇到的问题和解决方案。

<!-- more -->

## 环境信息

- **操作系统**: macOS (Darwin)
- **架构**: Apple Silicon (ARM64)
- **Shell**: bash
- **包管理器**: Homebrew

## 系统要求

### 必需工具

1. **GNU Make 4+**
   ```bash
   brew install make
   ```

2. **Erlang 27.x**
   ```bash
   brew install erlang@27
   ```

3. **p7zip (7z 命令)**
   ```bash
   brew install p7zip
   ```

4. **Git**
   ```bash
   brew install git
   ```

### 可选工具

- **Elixir** (用于 CLI 工具)
- **rebar3** (Erlang 构建工具)

## 编译过程

### 1. 环境准备

```bash
# 克隆 RabbitMQ 服务器源码
git clone https://github.com/rabbitmq/rabbitmq-server.git
cd rabbitmq-server

# 设置环境变量
export PATH="/opt/homebrew/opt/erlang@27/bin:/opt/homebrew/bin:$PATH"
```

### 2. 清理和编译

```bash
# 清理所有构建产物
git clean -xfffd
gmake clean
gmake distclean

# 开始编译（推荐使用单线程避免并发问题）
gmake -j1
```

## 遇到的问题和解决方案

### 问题 1: Erlang 版本不兼容

**错误信息**:
```
rabbit_data_coercion.erl:76:23: syntax error before: '||'
```

**原因**: RabbitMQ 4.x 使用了 Erlang 26+ 才支持的 map comprehension 语法，而系统安装的是 Erlang 25。

**解决方案**:
```bash
# 卸载旧版本 Erlang
brew uninstall erlang@25

# 安装 Erlang 27
brew install erlang@27

# 更新 PATH
export PATH="/opt/homebrew/opt/erlang@27/bin:$PATH"
```

**验证**:
```bash
erl -version
# 应该显示: Erlang (SMP,ASYNC_THREADS) (BEAM) emulator version 15.2.7.4

# 测试 map comprehension 语法
erl -eval 'M = #{a => 1, b => 2}, R = #{K => V || K := V <- M}, io:format("~p~n", [R]), halt().' -noshell
# 应该输出: #{a => 1,b => 2}
```

### 问题 2: wxWidgets 兼容性问题

**错误信息**:
```
observer:start().
=CRASH REPORT==== Symbol not found: __ZNK8wxCursor10GetHotSpotEv
```

**原因**: Erlang 27 与现有的 wxWidgets 版本不兼容。

**解决方案**:
```bash
# 卸载 Erlang 27 和 wxWidgets
brew uninstall erlang@27
brew uninstall --ignore-dependencies wxwidgets

# 重新安装（这会自动安装兼容的 wxWidgets 版本）
brew install erlang@27
```

**验证**:
```bash
# 测试 observer 是否正常工作
erl -eval 'observer:start(), timer:sleep(2000), observer:stop(), halt().'
# 应该正常启动和关闭，无错误信息
```

### 问题 3: 缺少 7z 命令

**错误信息**:
```
/bin/sh: 7z: command not found
gmake[2]: *** [../../erlang.mk:3510: escript-zip] Error 127
```

**原因**: RabbitMQ CLI 工具构建需要 7z 命令来创建 escript 归档。

**解决方案**:
```bash
# 安装 p7zip
brew install p7zip

# 验证安装
which 7z
# 应该输出: /opt/homebrew/bin/7z
```

### 问题 4: hex_core 依赖构建失败

**错误信息**:
```
gmake[2]: *** No targets specified and no makefile found.  Stop.
touch: cannot touch '/Users/.../deps/hex_core/ebin/dep_built': No such file or directory
```

**原因**: hex_core 使用 rebar3 构建系统，但 erlang.mk 尝试用 make 构建它。

**解决方案**:
```bash
# 手动编译 hex_core
cd deps/hex_core
mkdir -p ebin
erlc -o ebin src/*.erl
cp src/hex_core.app.src ebin/hex_core.app
touch ebin/dep_built
cd ../..
```

### 问题 5: ra 模块并发编译问题

**错误信息**:
```
failed to rename .../ra_snapshot.bea# to .../ra_snapshot.beam: no such file or directory
```

**原因**: 并发编译时文件操作冲突。

**解决方案**:
```bash
# 使用单线程编译
gmake -j1

# 或者清理 ra 目录后重新编译
rm -rf deps/ra/ebin
mkdir deps/ra/ebin
gmake -j1 deps
```

### 问题 6: Generic Unix 包构建中的重复文件名错误

**错误信息**:
```
ERROR:
Duplicate filename on disk:
dep_built
dep_built
make[6]: *** [../../erlang.mk:3511: escript-zip] Error 2
```

**原因**: 在创建 escript zip 文件时，多个依赖项中都包含 `dep_built` 文件，导致 7z 归档时文件名冲突。

**解决方案**:

方法1 - 临时删除 dep_built 文件：
```bash
# 删除所有 dep_built 文件
find deps -name "dep_built" -delete

# 然后构建包
gmake package-generic-unix
```

方法2 - 修改 erlang.mk（永久解决）：
```bash
# 在 erlang.mk 第3510-3515行，添加 filter-out 来排除 dep_built 文件
# 将：
#   $(subst $(DEPS_DIR)/,,$(addsuffix /*,$(wildcard \
# 改为：
#   $(filter-out %/dep_built,$(subst $(DEPS_DIR)/,,$(addsuffix /*,$(wildcard \
```

方法3 - 使用构建脚本：
```bash
# 使用提供的自动化脚本
./build_package.sh
```

**验证**:
```bash
# 检查生成的包文件
ls -la PACKAGES/rabbitmq-server-*.tar.xz
```

## 包构建脚本

创建 `build_package.sh` 脚本来自动化包构建过程：

```bash
#!/bin/bash
# RabbitMQ Generic Unix Package Build Script
set -e

echo "开始构建 RabbitMQ Generic Unix 包..."

# 设置环境变量
export PATH="/opt/homebrew/opt/erlang@27/bin:/opt/homebrew/bin:$PATH"

# 验证环境
echo "验证构建环境..."
which gmake || { echo "错误: 未找到 gmake"; exit 1; }
which erl || { echo "错误: 未找到 erlang"; exit 1; }
which 7z || { echo "错误: 未找到 7z"; exit 1; }

# 检查 Erlang 版本
ERL_VERSION=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell)
if [[ "$ERL_VERSION" < "27" ]]; then
    echo "错误: 需要 Erlang 27+，当前版本: $ERL_VERSION"
    exit 1
fi

echo "Erlang 版本: $ERL_VERSION ✓"

# 清理可能导致问题的 dep_built 文件
echo "清理 dep_built 文件..."
find deps -name "dep_built" -delete 2>/dev/null || true

# 确保主项目已经构建
echo "确保主项目已构建..."
if [ ! -f "sbin/rabbitmq-server" ]; then
    echo "主项目未构建，开始构建..."
    gmake -j1
fi

# 构建 generic-unix 包
echo "构建 Generic Unix 包..."
gmake package-generic-unix

echo "构建完成！"

# 检查输出
PACKAGE_DIR="PACKAGES"
if [ -d "$PACKAGE_DIR" ]; then
    echo "生成的包文件:"
    ls -la "$PACKAGE_DIR"/rabbitmq-server-*.tar.xz 2>/dev/null || echo "未找到 .tar.xz 文件"
    ls -la "$PACKAGE_DIR"/rabbitmq-server-*.tar.gz 2>/dev/null || echo "未找到 .tar.gz 文件"
else
    echo "警告: 未找到 PACKAGES 目录"
fi
```

## 完整编译脚本

```bash
#!/bin/bash

# RabbitMQ 服务器编译脚本
set -e

echo "开始编译 RabbitMQ 服务器..."

# 设置环境变量
export PATH="/opt/homebrew/opt/erlang@27/bin:/opt/homebrew/bin:$PATH"

# 验证必需工具
echo "验证构建工具..."
which gmake || { echo "错误: 未找到 gmake"; exit 1; }
which erl || { echo "错误: 未找到 erlang"; exit 1; }
which 7z || { echo "错误: 未找到 7z"; exit 1; }

# 检查 Erlang 版本
ERL_VERSION=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell)
if [[ "$ERL_VERSION" < "27" ]]; then
    echo "错误: 需要 Erlang 27+，当前版本: $ERL_VERSION"
    exit 1
fi

echo "Erlang 版本: $ERL_VERSION ✓"

# 清理构建产物
echo "清理构建产物..."
git clean -xfffd
gmake clean
gmake distclean

# 手动构建 hex_core（如果需要）
if [ ! -f "deps/hex_core/ebin/dep_built" ]; then
    echo "手动构建 hex_core..."
    cd deps/hex_core
    mkdir -p ebin
    erlc -o ebin src/*.erl 2>/dev/null || true
    cp src/hex_core.app.src ebin/hex_core.app 2>/dev/null || true
    touch ebin/dep_built
    cd ../..
fi

# 开始编译
echo "开始编译 RabbitMQ..."
gmake -j1

echo "编译完成！"

# 验证编译结果
echo "验证编译结果..."
if [ -f "sbin/rabbitmq-server" ] && [ -f "escript/rabbitmqctl" ]; then
    echo "✓ 编译成功！"
    echo "  - 服务器脚本: sbin/rabbitmq-server"
    echo "  - 控制工具: escript/rabbitmqctl"
else
    echo "✗ 编译可能不完整，请检查错误信息"
    exit 1
fi
```

## 验证编译结果

### 检查生成的文件

```bash
# 检查主要可执行文件
ls -la sbin/rabbitmq-server
ls -la escript/rabbitmqctl

# 检查插件目录
ls -la plugins/

# 检查 Erlang beam 文件
find deps/rabbit/ebin -name "*.beam" | head -5
```

### 运行测试

```bash
# 启动开发模式服务器
gmake run-broker

# 在另一个终端中测试连接
./escript/rabbitmqctl status
```

## 常见问题排查

### 编译时间过长

- **原因**: RabbitMQ 是大型项目，包含众多模块
- **解决**: 正常现象，完整编译需要 15-25 分钟

### 内存不足

- **症状**: 编译过程中系统卡顿或进程被杀死
- **解决**: 使用 `gmake -j1` 减少并发，或增加虚拟内存

### 权限问题

- **症状**: 无法创建文件或目录
- **解决**: 确保对项目目录有写权限，避免使用 sudo

### 网络问题

- **症状**: 依赖下载失败
- **解决**: 检查网络连接，考虑使用代理或镜像源

## 性能优化建议

1. **使用 SSD**: 确保项目在 SSD 上编译
2. **充足内存**: 推荐 8GB+ 内存
3. **单线程编译**: 使用 `-j1` 避免并发问题
4. **清理缓存**: 定期运行 `gmake distclean`

## 相关资源

- [RabbitMQ 官方文档](https://www.rabbitmq.com/docs/)
- [RabbitMQ GitHub 仓库](https://github.com/rabbitmq/rabbitmq-server)
- [Erlang/OTP 文档](https://www.erlang.org/docs)

## 版本信息

- **文档版本**: 1.0
- **RabbitMQ 版本**: 4.x (main 分支)
- **Erlang 版本**: 27.3.4.6
- **创建日期**: 2026-01-16
- **最后更新**: 2026-01-16

---

**注意**: 本文档基于 macOS Apple Silicon 环境编写，其他平台可能需要调整部分步骤。
