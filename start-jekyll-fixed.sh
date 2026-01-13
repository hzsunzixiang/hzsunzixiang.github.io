#!/bin/bash

echo "🚀 启动 Jekyll 本地开发服务器（修复版）"
echo "========================================="

# 检查端口是否被占用
check_port() {
    if lsof -Pi :$1 -sTCP:LISTEN -t >/dev/null 2>&1; then
        echo "⚠️  端口 $1 被占用，尝试杀死进程..."
        lsof -ti:$1 | xargs kill -9 2>/dev/null || true
        sleep 2
    fi
}

# 清理之前的构建
echo "🧹 清理之前的构建文件..."
rm -rf _site .jekyll-cache .jekyll-metadata

# 检查并清理端口
check_port 4000
check_port 35729

# 启动 Jekyll 服务器
echo "🌟 启动 Jekyll 服务器..."
echo "📍 访问地址: http://localhost:4000"
echo "🔄 LiveReload: http://localhost:35729"
echo "🛑 按 Ctrl+C 停止服务器"
echo ""

# 使用本地配置文件启动
if [ -f "_config_local.yml" ]; then
    echo "📝 使用本地配置文件..."
    bundle exec jekyll serve \
        --config _config.yml,_config_local.yml \
        --host 0.0.0.0 \
        --port 4000 \
        --livereload \
        --livereload-port 35729 \
        --incremental \
        --drafts \
        --future \
        --force_polling
else
    echo "📝 使用默认配置..."
    bundle exec jekyll serve \
        --host 0.0.0.0 \
        --port 4000 \
        --livereload \
        --livereload-port 35729 \
        --incremental \
        --drafts \
        --future \
        --force_polling
fi
