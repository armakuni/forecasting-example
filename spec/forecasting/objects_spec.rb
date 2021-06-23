require 'tempfile'

describe Forecasting do
  describe '.to_object_array' do
    subject do
      arr = [
        { 'foo' => 'bar' }
      ]
      Forecasting.to_object_array(arr)
    end
    it 'converts objects to OpenStruct array' do
      expect(subject).to be_a Array
      expect(subject.size).to eq 1
      expect(subject[0]).to be_a OpenStruct
      expect(subject[0].foo).to eq 'bar'
    end
  end

  describe '.to_object_hash' do
    subject do
      arr = [
        { 'complexity' => 1, 'foo' => 'bar' }
      ]
      Forecasting.to_object_hash(arr, 'complexity')
    end
    it 'converts objects to OpenStruct hash, keyed correctly' do
      expect(subject).to be_a Hash
      expect(subject.size).to eq 1
      expect(subject[1]).to be_a OpenStruct
      expect(subject[1].foo).to eq 'bar'
    end
  end

  describe '.load_backlog' do
    let(:backlog_file) do
      Tempfile.new('backlog').tap do |f|
        f.write <<-EOS
          backlog:
          - name: task 1
            complexity: 1
        EOS
        f.close
      end
    end
    after { backlog_file.unlink }
    subject { Forecasting.load_backlog(backlog_file) }
    it 'loads the backlog file' do
      expect(subject).to be_a Array
      expect(subject.size).to eq 1
    end
  end

  describe '.load_performance' do
    let(:performance_file) do
      Tempfile.new('perf').tap do |f|
        f.write <<-EOS
          performance:
          - complexity: 1
            distribution: uniform
        EOS
        f.close
      end
    end

    after { performance_file.unlink }
    subject { Forecasting.load_performance(performance_file) }
    it 'loads the performance file' do
      expect(subject).to be_a Hash
      expect(subject[1]).to be_a OpenStruct
      expect(subject[1].distribution).to eq 'uniform'
    end
  end
end
