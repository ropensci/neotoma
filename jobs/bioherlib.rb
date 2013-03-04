require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get("http://www.biodiversitylibrary.org/api2/httpquery.ashx?op=GetPageMetadata&apikey=ea5bbfda-e6ad-4fce-b288-70fa9410a9b3&pageid=1328690")

	if result['Response']['Status'] == "ok"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('bhl', { text: out })
end