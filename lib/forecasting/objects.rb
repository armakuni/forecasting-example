module Forecasting
    def self.load_backlog(filename)
        backlog = YAML.safe_load(File.read(filename))['backlog'].map { |b| OpenStruct.new(b) }
    end

    def self.load_performance(filename)
        {}.tap do |performance|
            YAML.safe_load(File.read(filename))['performance'].each do |e|
              performance[e['complexity']] = OpenStruct.new(e)
            end
        end
    end
end
