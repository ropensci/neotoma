require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get("http://api.figshare.com/v1/articles/138")

	if result['items'][0]['article_id'] == 138
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('figshare', { text: out })
end