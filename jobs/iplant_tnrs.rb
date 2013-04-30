require 'httparty'
require 'json'

SCHEDULER.every '60s', :first_in => 0 do |job|
  # result = HTTParty.get('http://taxosaurus.org/sources/list')
  result = HTTParty.get('http://sharp.iplantcollaborative.org/submit?query=Panthera+tigris%0AEutamias+minimus%0AMagnifera+indica%0AHumbert+humbert')
  out = JSON.parse(result)['message']
  jobid = out.split(' ')[1]

  while tt == 'found'  do
  	sleep 1.5
  	result2 = HTTParty.get('http://sharp.iplantcollaborative.org/retrieve/' + jobid)
  	tt = JSON.parse(result2)['status']
  end

  if JSON.parse(result2)['names'][1]['matches'][1]['acceptedName'] == "Panthera tigris"
  	out = "BOOM! It's up"
  else
  	out = "Sad. It's down"
  end
  
  send_event('iplant_tnrs', { text: out })
end

