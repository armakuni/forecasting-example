describe Distribution::Triangular do
  describe '.random' do
    it 'returns a random number within bounds' do
      expect(described_class.random(10.0, 5.0, 15.0)).to be_between(5.0, 15.0)
    end

    it 'converges towards the mode for large sample sizes' do
      sum = 0.0
      10_000.times do
        sum += described_class.random(10.0, 5.0, 15.0)
      end
      expect(sum / 10_000).to be_within(0.5).of(10.0)
    end
  end

  # TODO: it would be nice to do some goodness of fit test to validate the random
  # values approximate the appropriate distribution curve.
end
