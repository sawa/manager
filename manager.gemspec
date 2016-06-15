Gem::Specification.new do
	|s|
	s.name = "manager"
	s.version = "0.1.2"
	s.date = "2016-05-14"
	s.authors = ["sawa"]
	s.email = []
	s.license = "MIT"
	s.summary = "Documentation and test framework for software written in Ruby"
	s.description = "Manager generates a user's manual and a developer's chart simultaneously from a single spec file that contains both kinds of information. More precisely, it is a document generator, source code annotation extracter, source code analyzer, class diagram generator, unit test framework, benchmark measurer for alternative implementations of a feature, all in one."
	s.homepage = "http://www.rubymanager.com/"
	s.metadata = {
		"issue_tracker" => "https://github.com/sawa/manager"
	}
	s.executables << "manager"
	s.files = Dir["lib/**/*"] + Dir["theme/**/*"] + Dir["spec/**/*"]
	s.platform = Gem::Platform::RUBY
	s.required_ruby_version = ">= 2.3.1"
	s.add_runtime_dependency "benchmark-ips", ">= 0"
	s.add_runtime_dependency "dom", ">= 1.0.0"
	s.add_runtime_dependency "coderay", ">= 0"
	s.add_development_dependency "manager", ">= 0"
	s.requirements << "ffi-aspell gem (optional), a web browser compatible with HTML5 and CSS3"
end
