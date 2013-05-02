require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://ebird.org/ws1.1/data/obs/geo_spp/recent?lng=-70.51&lat=42.4&sci=branta%20canadensis&fmt=json')
  result = HTTParty.get('http://ebird.org/ws1.1/data/obs/geo_spp/recent?lng=-76.51&lat=45.46&sci=branta%20canadensis&fmt=json')
  						 
  
	if result[0]['comName'] == "Canada Goose"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('ebird', { text: out })
end