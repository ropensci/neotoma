require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get("https://www.usanpn.org/npn_portal/observations/getAllObservationsForSpecies.json?species_id[0]=52&start_date=2011-01-01&end_date=2011-02-01")

	if result['station_list'][0]['station_id'] == "4881"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('npn', { text: out })
end