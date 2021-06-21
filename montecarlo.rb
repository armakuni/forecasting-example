require 'pathname'
require 'optparse'

$: << Pathname.new(__FILE__).parent + 'lib'
require 'forecasting'

backlog_file = ''
performance_file = ''
iterations = 1000
workers = 1

OptionParser.new do |opts|
  opts.on('-b', '--backlog FILE', 'File containing backlog items') { |p| backlog_file = p }
  opts.on('-p', '--performance FILE', 'File containing performance data') { |p| performance_file = p }
  opts.on('-i', '--iterations COUNT', Integer, 'Number of iterations to run') { |p| iterations = p }
  opts.on('-w', '--workers COUNT', Integer, 'Number of backlog workers') { |p| workers = p }
end.parse!

abort 'No backlog file specified' unless File.exist?(backlog_file)
abort 'No performance file specified' unless File.exist?(performance_file)

backlog = Forecasting.load_backlog(backlog_file)
performance = Forecasting.load_performance(performance_file)

simulator = Forecasting::MonteCarlo.new(performance)
simulator.run(backlog: backlog, iterations: iterations, workers: workers)
simulator.report
