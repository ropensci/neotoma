require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get("api.neotomadb.org/v1/data/sites/1")

	if result[0]['success'] == '1'
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('neotoma', { text: out })
end