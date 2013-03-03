require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://resolver.globalnames.org/name_resolvers.json?names=Plantago+major&data_source_ids=1')
  
	if result['context'][0]['context_clade'] == "Plantago major"
			# out = "BOOM! It's up"
			out = 100
		else
			# out = "Sad face. It's down"
			out = 0
		end
  
  send_event('eol_gnr', { value: out })
end