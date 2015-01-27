require 'bundler/setup'

require 'albacore'
require 'albacore/tasks/release'
require 'albacore/tasks/versionizer'
require 'albacore/ext/teamcity'

Configuration = ENV['CONFIGURATION'] || 'Release'

Albacore::Tasks::Versionizer.new :versioning

namespace :z3 do
  task :ensure_python do
    system 'which python' do |ok, res|
      $stderr.puts "no python installed, please install python" unless ok
    end
  end

  task :module do
    sh 'git submodule update --init'
  end

  task :compile => :module do
    Dir.chdir 'vendor/Z3' do
      sh 'CXX=clang++ CC=clang python scripts/mk_make.py --dotnet'
      Dir.chdir 'build' do
        sh 'make'
      end
    end
  end
end

task :z3 => [:'z3:compile']

desc 'create assembly infos'
asmver_files :assembly_info do |a|
  a.files = FileList['src/**/*.fsproj']

  a.attributes assembly_description: 'A Z3 F# API wrapper',
               assembly_configuration: Configuration,
               assembly_company: 'Foretag AB',
               assembly_copyright: "(c) 2016 by Phan Anh Dung & Henrik Feldt",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']
end

desc 'Perform fast build (warn: doesn\'t d/l deps)'
build :quick_compile do |b|
  b.prop 'Configuration', Configuration
  b.logging = 'detailed'
  b.sln     = 'src/FsZ3.Mono.sln'
end

task :paket_bootstrap do
system 'tools/paket.bootstrapper.exe', clr_command: true unless   File.exists? 'tools/paket.exe'
end

desc 'restore all nugets as per the packages.config files'
task :restore => [:paket_bootstrap] do
  system 'tools/paket.exe', 'restore', clr_command: true
end

desc 'Perform full build'
build :compile => [:versioning, :restore, :assembly_info, :z3] do |b|
  b.prop 'Configuration', Configuration
  b.sln = 'src/FsZ3.Mono.sln'
end

directory 'build/pkg'

desc 'package nugets (without compiling) – finds all projects and package them'
nugets_pack :create_nugets_quick => [:versioning] do |p|
  p.configuration = Configuration
  p.files = FileList['src/**/*.fsproj'].exclude(/Tests/)
  p.out   = 'build/pkg'
  p.exe   = 'packages/NuGet.CommandLine/tools/NuGet.exe'
  p.with_metadata do |m|
    m.id          = 'FsZ3'
    m.title       = 'F# Z3 API wrapper'
    m.description = 'This is a F# Z3 API wrapper'
    m.authors     = '(c) 2017 by Phan Anh Dung, Henrik Feldt'
    m.project_url = 'https://github.com/haf/FsZ3'
    m.tags        = 'z3 fsharp constraint solver SAT'
    m.version     = ENV['NUGET_VERSION']
  end
end

desc 'package nugets – finds all projects and package them'
task :create_nugets => ['build/pkg', :versioning, :compile, :create_nugets_quick]

namespace :tests do
  #task :unit do
  #  system "src/MyProj.Tests/bin/#{Configuration}/MyProj.Tests.exe", clr_command: true
  #end
end

# task :tests => :'tests:unit'

task :default => :create_nugets #, :tests ]

task :ensure_nuget_key do
  raise 'missing env NUGET_KEY value' unless ENV['NUGET_KEY']
end

Albacore::Tasks::Release.new :release,
                             pkg_dir: 'build/pkg',
                             depend_on: [:create_nugets, :ensure_nuget_key],
                             nuget_exe: 'packages/NuGet.CommandLine/tools/NuGet.exe',
                             api_key: ENV['NUGET_KEY']
