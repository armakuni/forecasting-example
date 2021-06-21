describe 'Distribution.select' do
  it 'triangular returns a Triangular distribution' do
    expect(Distribution.select('triangular')).to be Distribution::Triangular
  end

  it 'pert returns a PERT Beta distribution' do
    expect(Distribution.select('pert')).to be Distribution::Pert
  end

  it 'uniform returns a Uniform distribution' do
    expect(Distribution.select('uniform')).to be Distribution::Uniform
  end

  it 'raises an error if invalid distribution requested' do
    expect { Distribution.select('foobar') }.to raise_error(/Unsupported/)
  end
end
