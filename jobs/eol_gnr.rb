require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://resolver.globalnames.org/name_resolvers.json?names=Plantago+major&data_source_ids=1')
  
	if result['data'][0]['supplied_name_string'] == "Plantago major"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('eol_gnr', { text: out })
end