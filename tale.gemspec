# coding: utf-8

Gem::Specification.new do |spec|
  spec.name          = "thenitinprakash.com"
  spec.version       = "0.2.3"
  spec.authors       = ["Nitin Prakash"]
  spec.email         = ["prakash.nitin63@gmail.com"]

  spec.summary       = %q{This blog is based on Tale theme by Chester How.}
  spec.homepage      = "https://thenitinprakash.com"
  spec.license       = "MIT"

  spec.files         = `git ls-files -z`.split("\x0").select { |f| f.match(%r{^(assets|_layouts|_includes|_sass|LICENSE|README)}i) }

  spec.add_runtime_dependency "jekyll", "~> 4.3.2"
  spec.add_runtime_dependency "jekyll-paginate", "~> 1.1"
  spec.add_runtime_dependency "jekyll-feed", "~> 0.17"
  spec.add_runtime_dependency "jekyll-seo-tag", "~> 2.8.0"

  spec.add_development_dependency "bundler", "~> 2.4"
  spec.add_development_dependency "rake", "~> 13.0.6"
end
