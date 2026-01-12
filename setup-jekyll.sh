#!/bin/bash

echo "🚀 Setting up Jekyll for local development..."

# Check if Homebrew is installed
if ! command -v brew &> /dev/null; then
    echo "❌ Homebrew not found. Please install Homebrew first:"
    echo "   /bin/bash -c \"\$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
    exit 1
fi

# Install Ruby using Homebrew (recommended for macOS)
echo "📦 Installing Ruby via Homebrew..."
brew install ruby

# Add Ruby to PATH
echo "🔧 Setting up Ruby PATH..."
echo 'export PATH="/opt/homebrew/opt/ruby/bin:$PATH"' >> ~/.zshrc
echo 'export PATH="/opt/homebrew/lib/ruby/gems/3.3.0/bin:$PATH"' >> ~/.zshrc

# Reload shell configuration
source ~/.zshrc

# Install Jekyll and Bundler
echo "💎 Installing Jekyll and Bundler..."
gem install --user-install bundler jekyll

# Install project dependencies
echo "📚 Installing project dependencies..."
bundle install

echo "✅ Jekyll setup complete!"
echo ""
echo "🌐 To run your blog locally:"
echo "   bundle exec jekyll serve"
echo ""
echo "📱 Then visit: http://localhost:4000"