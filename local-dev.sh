#!/bin/bash

echo "🌐 GitHub Pages Blog - Local Development Server"
echo "=============================================="

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Method 1: Try Jekyll
if command_exists jekyll; then
    echo "✅ Jekyll found! Starting Jekyll server..."
    bundle exec jekyll serve --livereload --host 0.0.0.0 --port 4000
    exit 0
fi

# Method 2: Try Docker
if command_exists docker; then
    echo "🐳 Jekyll not found, trying Docker..."
    if [ -f "docker-compose.yml" ]; then
        echo "📦 Starting Docker container..."
        docker-compose up
        exit 0
    else
        echo "🚀 Running Jekyll in Docker container..."
        docker run --rm -it \
            -p 4000:4000 \
            -p 35729:35729 \
            -v "$(pwd):/srv/jekyll" \
            jekyll/jekyll:4.2.2 \
            jekyll serve --watch --force_polling --host 0.0.0.0 --livereload
        exit 0
    fi
fi

# Method 3: Simple HTTP server (fallback)
echo "⚠️  Jekyll and Docker not available, using simple HTTP server..."
echo "📝 Note: This won't process Jekyll templates, only serves static files"

if command_exists python3; then
    echo "🐍 Starting Python HTTP server on port 8080..."
    python3 -m http.server 8080
elif command_exists python; then
    echo "🐍 Starting Python HTTP server on port 8080..."
    python -m SimpleHTTPServer 8080
elif command_exists node; then
    echo "📦 Installing and starting http-server..."
    npx http-server -p 8080 -o
else
    echo "❌ No suitable server found. Please install one of:"
    echo "   - Jekyll: gem install jekyll bundler"
    echo "   - Docker: https://docker.com"
    echo "   - Python: python3 -m http.server"
    echo "   - Node.js: npx http-server"
    exit 1
fi