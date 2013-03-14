require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('https://api.github.com/repos/ropensci/rmendeley/forks')
  
	if result[0]['name'] == "RMendeley"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('github', { text: out })
end