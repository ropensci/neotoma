require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('https://fluiddb.fluidinfo.com/values?query=elifesciences.org/api_v1/article/doi=%2210.7554/eLife.00013%22&tag=*')
  
	if result['results']['id']['eb275979-d99d-4e44-a84f-15ce912ca888']['fluiddb/about']['username'] == "fluiddb"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('elife', { text: out })
end