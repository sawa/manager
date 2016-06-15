# frozen_string_literal: true

# Copyright (c) 2016 sawa

# stdlib
require "pathname"
require "stringio"
require "objspace"
require "coverage"

# gems
require "benchmark/ips"
require "dom"
require "coderay"

# project files
load("#{__dir__}/manager/refine_module")
load("#{__dir__}/manager/refine_test")
load("#{__dir__}/manager/input")
load("#{__dir__}/manager/test")
load("#{__dir__}/manager/annotation")
load("#{__dir__}/manager/render")
load("#{__dir__}/manager/refine_object_mapping")
load("#{__dir__}/manager/spell_check")
load("#{__dir__}/manager/test_helper")

class Manager
	DebugDirs = [
		$0,
		File.expand_path("#{__dir__}/../bin"),
		__dir__,
		Gem.loaded_specs["benchmark-ips"].gem_dir,
	]
	Gem.loaded_specs.clear
	Main = TOPLEVEL_BINDING.receiver
	#! This format must be applicable to methods whose normal form ends with `?`, `!`, or `=`.
	# Also, it must save the alternative part like `2` from being in the first part of the method, to
	#  avoid syntax error.
	AlternativeMethod = /\A(?!.*__.+__)(.+)__[^?!=]+([?!=]?)\z/
	InterruptionInactive = Proc.new{print "\b\b\b\b\b"}

	using ModuleRefinement
	using ObjectMappingRefinement
	using TesterRefinement

	class Spec
		attr_reader :i, :items
		attr_accessor :header, :documentation, :hidden, :type, :aliases, :alts
		def initialize; @hidden, @alts, @items = false, {}, [] end
		def undocumented_mark
			@undocumented = true
			self
		end
		def order_fix; @i = Manager.counts[:spec] += 1 end
		def missing? type, module_documentation, module_hidden
			#! Cannot replace `===` with `kind_of?` because `e` may be a `SanitizedObject`.
			@type = Render::Tag.new(module_hidden || @hidden, nil, type.to_s)
			if module_documentation.! and @undocumented
				@documentation = Render::Tag.new(true, "Undocumented")
			elsif module_hidden.! and @hidden.!
				if @items.none?{|e| Manager::UnitTest === e}
					@items.push(Render::BulletWrapper.new("Missing test", message: "Not unit tested."))
				end
				if @items.none?{|e| Render::UserParagraph === e}
					@items.push(Render::BulletWrapper
					.new("Missing Doc", message: "No paragraph given for user."))
				elsif @items.none?{|e| Render::MethodSignature === e}
				   case type; when :singleton, :instance
						@items.push(Render::BulletWrapper
						.new("Missing Doc", message: "No method signature given."))
					end
				end
			end
		end
	end

	singleton_class.class_eval{attr_accessor :current}
	def self.context; current.context end
	def self.counts; current.counts end
	@config = {}
	def self.config k = nil, bdir: nil, bdir_expanded: nil, odir: nil, user: nil, dev: nil, theme: nil, highlight: nil, debug: nil, spell_check: nil, case_sensitive: nil, case_insensitive: nil, spell_checker: nil, spell_check_regex: nil, timeout: nil, title: nil, coverage: nil #! `coverage` is a temporal workaround
		return @config[k] if k
		@config[:bdir] = bdir if bdir
		@config[:bdir_expanded] = Pathname.new(bdir_expanded) if bdir_expanded
		@config[:odir] = odir if odir
		@config[:user] = user if user
		@config[:dev] = dev if dev
		@config[:theme] = File.expand_path("#{__dir__}/../theme/#{theme}") if theme
		@config[:highlight] = File.expand_path("#{__dir__}/../theme/#{highlight}") if highlight
		@config[:debug] = debug if debug == true or debug == false
		if spell_check
			spell_check = spell_check.to_s
			if Spellcheck.language?(spell_check)
				@config[:spell_check] = spell_check
			else
				raise "The configured spell checking language `#{spell_check}` is not available. "\
				"Either install the corresponding aspell language component, or change "\
				"the `spell_check` option."
			end
		end
		if case_sensitive
			@config[:case_sensitive] =
			case_sensitive.each_with_object({}){|w, h| h[w] = true}
		end
		if case_insensitive
			@config[:case_insensitive] =
			case_insensitive.each_with_object({}){|w, h| h[w.downcase] = true}
		end
		@config[:spell_checker] = spell_checker if spell_checker
		@config[:spell_check_regex] = spell_check_regex if spell_check_regex
		@config[:timeout] = timeout if timeout
		@config[:title] = title if title
		@config[:coverage] = coverage
		nil
	end
	attr_accessor :files, :implementations, :annotations, :context, :counts, :annotation_extractor
	attr_reader :slf, :sample, :modul, :type, :feature
	def initialize spec, **command_options
		Signal.trap("INT", &InterruptionInactive)
		@files, @specs, @described_headers, @implementations, @annotations, @context, @counts =
		[], {}, {}, {}, {}, Context.new, Hash.new(0)
		self.class.current = self
		abort "Spec file not found" unless spec.kind_of?(String)
		abort "Spec file not found: #{spec}" unless File.exist?(spec)
		spec_s, _, data = File.read(spec).partition(/^__END__[ \t]*\n/)
		l = spec_s.count($/)
		@files.push([spec, {mtime: File.new(spec).mtime, lines: l}])
		manage(spec, data, l) unless data.empty?
		Coverage.start
		ObjectSpace.trace_object_allocations_start
		begin
			load(spec)
		rescue Exception => e
			Console.abort(e)
		ensure
			ObjectSpace.trace_object_allocations_stop
		end
		#! command line options override options given in spec files
		Manager.config(command_options)
		bdir = File.expand_path(Manager.config(:bdir), File.dirname(spec))
		Console.abort("Configured \"#{bdir}\" is not a directory") unless File.directory?(bdir)
		Manager.config(bdir_expanded: bdir)
		odir = File.expand_path(Manager.config(:odir), File.dirname(spec))
		Console.abort("Configured \"#{odir}\" is not a directory") unless File.directory?(odir)
		title = Manager.config(:title) || (@files.dig(1, 0) && File.basename(@files[1][0], ".*"))
		print "Combining spec with information gathered from program..."; $stdout.flush
		@implementations.each do
			|(modul, type, alt), location|
			@specs[modul] ||= {[:module, nil] => Spec.new.undocumented_mark}
			next if type == :module
			feature = Manager.main_method(alt)
			begin @specs[modul][[type, feature]] ||=
			case type
			when :module_as_constant then Spec.new
			when :instance, :singleton, :constant then Spec.new.undocumented_mark
			end end
			.alts[alt] = nil
		end
		@annotations.each do
			|(modul, type, feature), h|
			global = modul == Main
			if feature == nil
				@specs[modul] ||= {[:module, nil] => Spec.new}
				search_items = @specs[modul]
				.each_with_object([]){|((type, _), spec), a| a.concat(spec.items) if type == :module}
				#! Global annotations have the file name for `type`.
				#   Local `main` annotations have `nil` for `type`.
				target_items = @specs[modul][[:module, feature]].items
			else
				search_items =
				target_items = @specs[modul][[type, feature]].items
			end
			h.each do
				|tag, (locations, text)|
				search_items.any?{|e| Render::Annotation.concat?(e, global, tag, locations, text)} or
				target_items.push(Render::Annotation.new(global, tag, locations, text))
			end
		end
		print "done\r"; $stdout.flush
		print "Organizing items..."; $stdout.flush
		@specs[Object] &.reject! do
			|(type, _), spec| type == :module_as_constant and spec.items.empty?
		end
		@specs.delete(Object) if begin
			@specs[Object] &.length == 1 and @specs[Object][[:module, nil]].items.empty?
		end
		@specs.each do
			|modul, h|
			unless modul == Main
				#! Main only has `:module` item. `aliases` is used only with
				#    `:instance` and `:singleton` items.
				aliases = {instance: modul.aliases, singleton: modul.singleton_class.aliases}
				#! Must refer from child to ancestors, not the other way around because it is not 
				#   guaranteed that the child is ordered later than the ancestors within `@specs`.
				namespaces = modul.namespace
				module_documentation = namespaces
				.find{|m| break e if e = @specs.dig(m, [:module, nil]) &.documentation}
				module_hidden = namespaces
				.find{|m| break e if e = @specs.dig(m, [:module, nil]) &.hidden}
				h[[:module, nil]].hidden = true if module_hidden
			end
			h.each do
				|(type, feature), spec|
				if spec.hidden
					spec.items.each do
						|e| case e; when Render::UserItem
							Console.abort wrong_item(
							e.source_item || e, "A hidden feature has a user document item")
						end
					end
				end
				spec.order_fix
				case type
				when :module
					case feature
					when Render::DescribedHeader
						spec.header = feature
					when nil
						if spec.hidden
							h.each do |_, spec| spec.items.each do
								|e| case e; when Render::UserItem
									Console.abort wrong_item(
									e.source_item || e, "A hidden feature has a user document item")
								end
							end end
						end
						case modul
						when Main
							spec.header = Render::NilHeader
						else
							visibility = modul.constant_visibility(nil)
							spec.header =
							(spec.hidden ?
							Render::DevModuleHeader : Render::UserModuleHeader)
							.new(modul, spec, visibility)
							spec.items.unshift(Render::AncestorDiagrams.new(modul))
						end
					end
				when :module_as_constant
					spec.type = nil
					spec.alts[feature] = nil
					spec.alts.each_key{|alt| spec.alts[alt] = modul.const_get(alt.to_s)}
					spec.header =
					(module_hidden || spec.hidden ?
					Render::DevFeatureHeader : Render::UserFeatureHeader)
					.new(modul, type, feature, spec)
				when :constant
					spec.missing?(type, module_documentation, module_hidden)
					spec.alts[feature] = nil
					spec.alts.each_key{|alt| spec.alts[alt] =
					  modul.constant_value(alt, module_hidden || spec.hidden)}
					visibility = spec.alts[feature] &.visibility
					location = spec.alts[feature] &.location
					inherited = visibility && modul.inherited?(type, feature)
					#! Unlike methods, unknown (i.e. `location == nil`) does not count as `different_file`.
					#   It may or may not belong to an unlisted source location, but as of now,
					#   there is no way to tell.
					different_file = visibility && location && (@files.assoc(location &.[](0)).! || nil)
					spec.documentation ||= (inherited || different_file) && module_documentation.! &&
					Render::Tag.new(true, "Misplaced", message: [
						(inherited && "Defined on `#{inherited.inspect}`."),
						(different_file && "Not defined in a listed file."),
					].join(" "))
					spec.header =
					(module_hidden || spec.hidden ?
					Render::DevFeatureHeader : Render::UserFeatureHeader)
					.new(modul, type, feature, spec)
					spec.items.unshift(
						spec.alts.values.first,
						Render::Assignment.new(visibility, location)
					)
				when :instance, :singleton
					spec.missing?(type, module_documentation, module_hidden)
					spec.aliases = aliases.dig(type, feature)
					if spec.alts.key?(feature).!
						spec.alts[feature] = nil
						spec.alts = spec.alts.sort.to_h
					end
					spec.alts.each_key{|alt| spec.alts[alt] = modul.implementation(type, alt)}
					visibility = spec.alts[feature] &.visibility
					location = spec.alts[feature] &.location
					inherited = visibility && modul.inherited?(type, feature)
					different_file = visibility && (@files.assoc(location &.[](0)).! || nil)
					original_name = visibility && modul.original_name(type, feature)
					aliasing =  (original_name unless original_name == feature) || nil
					spec.documentation ||= (inherited || different_file || aliasing) &&
					module_documentation.! &&
					Render::Tag.new(true, "Misplaced", message: [
						(inherited && "Defined on `#{inherited.inspect}`."),
						(different_file && "Not defined in a listed file."),
						(aliasing && "Defined as alias of `#{aliasing}`.")
					].join(" "))
					spec.header =
					(module_hidden || spec.hidden ?
					Render::DevFeatureHeader : Render::UserFeatureHeader)
					.new(modul, type, feature, spec)
					spec.items.unshift(Render::Implementations.new(spec.alts.values))
				end
				#! The first key (the main key) can potentially have a `nil` value.
				test_alts = spec.alts.any?{|_, v| v} ? spec.alts.select{|_, v| v} : spec.alts
				@context.new_feature(modul, type, test_alts.keys)
				#! Symbol to proc cannot be used here because `evaluate` is refinement
				spec.items.map!{|e| e.evaluate}
			end
		end
		@top = Render::Top.new
		ObjectSpace.trace_object_allocations_clear
		print "Processing coverage..."; $stdout.flush
		@coverage = Coverage.result
		if Manager.config(:coverage)
			#! Drop the spec file
			@files.drop(1).each do
				|f, _|
				puts f
				l_i = @coverage[f].length.to_s.length
				l_s = @coverage[f].compact.max.to_s.length
				puts(@coverage[f].zip(File.readlines(f)).map.with_index(1) do
					|(n, l), i|
					s = (n || "-").to_s.rjust(l_s)
					s =
					case n
					when 0 then "\e[31m#{s}\e[m"
					else "\e[32m#{s}\e[m"
					end
					"#{i.to_s.rjust(l_i)} #{s} #{l}"
				end)
			end
		end
		print "done\r"; $stdout.flush
		print "Rendering user's manual...\r"; $stdout.flush
		size = File.write(user = "#{odir}/#{Manager.config(:user)}", render(mode: :user, title: title))
		puts "Wrote #{user} (#{size.to_s.gsub(/(?<=\d)(?=(?:\d{3})+\z)/, ",")} bytes)."
		print "Rendering developer's chart...\r"; $stdout.flush
		size = File.write(dev = "#{odir}/#{Manager.config(:dev)}", render(mode: :dev, title: title))
		puts "Wrote #{dev} (#{size.to_s.gsub(/(?<=\d)(?=(?:\d{3})+\z)/, ",")} bytes)."
	end
	def gemspec f; @gemspec = Gem::Specification.load(f) end
	def manage f, data = nil, l = nil
		#! The ordering between the following two lines is important. If done otherwise,
		#   the same file called through different paths would be loaded multiple times.
		return Dir.entries(f).each{|f| manage(f)} if File.directory?(f) unless f.nil?
		return if data.nil? and @files.any?{|k, _| k == f}
		begin
			l ||= File.read(f) &.count($/) unless f.nil?
		rescue Errno::ENOENT => e
			raise "#{e.message}\n"\
			"Called from #{caller_locations[2].absolute_path}:#{caller_locations[2].lineno}"
		end
		@files.push([f, {mtime: f && File.new(f) &.mtime, lines: (data ? data.count($/) + 1 : l)}])
		#!`nil` denotes that "source unknown" methods will be immune from "misplaced" error.
		return if f.nil?
		@annotation_extractor = AnnotationExtractor.new(f)
		tracing_classes = {}
		constants_stack = []
		Object.module_start(constants_stack, tracing_classes)
		tp_module_start = TracePoint.new(:class) do
			|tp|
			modul, _f, l = tp.binding.receiver, tp.path, tp.lineno
			next unless _f == f
			@annotation_extractor.read_upto(modul, :module, nil, [_f, l])
			modul.module_start(constants_stack, tracing_classes)
		end
		tp_module_end = TracePoint.new(:end) do
			|tp|
			modul, _f, l = tp.binding.receiver, tp.path, tp.lineno
			next unless _f == f
			@annotation_extractor.read_upto(modul, :module_end, nil, [_f, l])
			modul.module_end(constants_stack)
		end
		tp_module_start.enable
		tp_module_end.enable
		begin
			#! `l` for the spec code lines, `1` for the `__END__` line, and `1` for starting at 1.
			data ? TOPLEVEL_BINDING.eval(data, f, l + 1 + 1) : load(f)
		rescue Exception => e
			Console.abort(e)
		end
		tp_module_start.disable
		tp_module_end.disable
		Object.module_end(constants_stack)
		tracing_classes.each_key do
			|modul|
			modul.singleton_class.__send__(:remove_method, :singleton_method_added)
			modul.singleton_class.__send__(:remove_method, :method_added)
		end
		@annotation_extractor.close
		nil
	end
	def hide spec
		bad_spec " `hide` must be followed by `spec`." unless spec.instance_of?(Spec)
		case @type
		when :module
			bad_spec " `hide` can only apply to a method or a constant (including module)."
		when :module_as_constant
			modul = @specs[@modul.const_get(@feature.to_s)][[:module, nil]]
			bad_spec " Conflicting `hide` and `move`." if modul.documentation and modul.hidden.!
			modul.documentation = Render::Tag.new(true, "Hidden")
			modul.hidden = true
			spec.hidden = true
		when :instance, :singleton, :constant
			bad_spec " Conflicting `hide` and `move`." if spec.documentation and spec.hidden.!
			spec.documentation = Render::Tag.new(true, "Hidden")
			spec.hidden = true
		end
		nil
	end
	def move spec
		bad_spec " `move` must be followed by `spec`." unless spec.instance_of?(Spec)
		case @type
		when :module
			bad_spec " `move` can only apply to a method or a constant (including module)."
		when :module_as_constant
			modul = @specs[@modul.const_get(@feature.to_s)][[:module, nil]]
			bad_spec " Conflicting `hide` and `move`." if modul.documentation and modul.hidden
			modul.documentation = Render::Tag.new(true, "Moved")
		when :instance, :singleton, :constant	
			bad_spec " Conflicting `hide` and `move`." if spec.documentation and spec.hidden
			spec.documentation = Render::Tag.new(true, "Moved")
		end
		nil
	end
	def _spec slf, type, feature, items
		modul = slf == Main ? Object : slf
		case type
		when :module
			@modul = slf
		when :instance, :singleton
			@modul = modul
			@sample = slf
		when :constant
			@modul = modul
			@sample = modul
			if Module === (m = (modul.const_get(feature.to_s) rescue nil))
				@specs[m] ||= {[:module, nil] => Spec.new}
				type = :module_as_constant
			end
		end
		h = @specs[@modul] ||= {[:module, nil] => Spec.new}
		@type, @feature = type, feature
		spec = h[[@type, @feature]] ||= Spec.new
		#! Cannot use symbol to proc because `to_manager_object` is refined.
		spec.items.concat(items.map{|item| item.to_manager_object})
		#! Return value to pass to `hide` or `move`.
		spec
	end
	module Coda; end
	def self.validate_feature_call modul, feature, coda
		#! When `coda` is an instance of `Manager::UnitTest`, which is a child of `BasicObject`, 
		# `coda ==` would not be defined. Hence this order. The `==` is not symmetric.
		current.bad_spec "Missing `coda` as the last argument" unless Coda == coda
		case feature
		when AlternativeMethod
			raise "The feature name crashes with alternative"\
			" method name format reserved by manager gem, and cannot be used: #{feature}"
		when /\A::(.+)/
			[:constant, $1.to_sym]
		when /\A\.(.+)/
			[:singleton, $1.to_sym]
		when /\A#(.+)/
			[:instance, $1.to_sym]
		when /\A=/
			feature.gsub!(/\A(=+) ?/, "")
			[:module, current.add_described_header(modul, $1.length, feature)]
		when nil
			[:module, nil]
		else
			raise current.wrong_item(feature, "Invalid feature name")
		end
	end
	def self.main_method sym
		AlternativeMethod.match(sym) &.captures &.join &.to_sym || sym
	end
	def i modul, type, feature
		@specs.dig(modul, [type, feature]) &.i
	end
	def add_described_header modul, depth, feature
		h = @described_headers[modul] ||= {}
		k = depth == 1 ? [] : h.reverse_each.find do
			|k, _|
			if k.length == depth and k.last == feature
				raise wrong_item(feature, "Described feature name crash")
			end
			k.length == depth - 1
		end &.first
		raise wrong_item(feature, "Invalid depth for described feature") unless k
		h[[*k, feature]] = Render::DescribedHeader.new(modul, depth, feature)
	end
	def described_headers modul, names_path
		@described_headers[modul]
		&.select{|k, _| k.last(names_path.length) == names_path}
		&.group_by{|k, _| k.length} &.min_by(&:first) &.last &.map(&:last)
	end
	def bad_spec message
		raise +"In" <<
		[
			@files.first.first,
			"#{caller.find{|l| l.start_with?(@files.first.first)} &.split(":") &.[](1)}",
			message,
		].join(":")
	end
	def wrong_item item, message
		RuntimeError.new((+"In ") <<
		[
			*if Manager.config(:debug) or Manager::DebugDirs
			.none?{|dir| ObjectSpace.allocation_sourcefile(item) &.start_with?(dir)}
				[ObjectSpace.allocation_sourcefile(item), ObjectSpace.allocation_sourceline(item)]
			else "?"
			end,
			message,
			item.inspect,
		].join(":"))
	end
end

Manager.config(
	bdir: "./",
	bdir_expanded: __dir__,
	odir: "./",
	user: "MANUAL.html",
	dev: "CHART.html",
	theme: "2016a.css",
	highlight: "coderay_github.css",
	debug: false,
	spell_check: nil,
	case_sensitive: [],
	case_insensitive: [],
)
Manager::CodeRayOption = {tab_width: 2, css: :class}
