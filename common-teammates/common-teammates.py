from tqdm import tqdm
from time import sleep
from nba_api.stats.static import players
from nba_api.stats.endpoints import playercareerstats, commonteamroster

def get_player_id(name):
	p = players.find_players_by_full_name(name)
	if len(p) == 0:
		raise ValueError('No name matching regex pattern: {}'.format(name))
	elif len(p) > 1:
		raise ValueError('Multiple players matching regex pattern: {}'.format(name))
	print 'Found player: {}'.format(p[0]['full_name'])
	return p[0]['full_name'], p[0]['id']


def get_szn_teammates(tid, szn):
	teammates = commonteamroster.CommonTeamRoster(team_id=tid, season=szn)
	return [d['PLAYER_ID'] for d in teammates.get_normalized_dict()['CommonTeamRoster']]


def get_common_teammates(p1name, p2name):
	'''
	Get all common teammates of the players listed
	'''

	print 'Getting players ...'
	p1name, p1id = get_player_id(p1name)
	p2name, p2id = get_player_id(p2name)

	print 'Getting careers ...'
	p1career = playercareerstats.PlayerCareerStats(p1id)
	sleep(0.2)
	p2career = playercareerstats.PlayerCareerStats(p2id)
	sleep(0.2)

	p1teamsszns = [(d['TEAM_ID'], d['SEASON_ID']) for d in p1career.get_normalized_dict()['SeasonTotalsRegularSeason'] if d['TEAM_ID'] != 0]
	p2teamsszns = [(d['TEAM_ID'], d['SEASON_ID']) for d in p2career.get_normalized_dict()['SeasonTotalsRegularSeason'] if d['TEAM_ID'] != 0]

	print 'Getting teammates ...'
	p1teammates = set()
	for tid, szn in tqdm(p1teamsszns, desc=p1name):
		p1teammates.update(get_szn_teammates(tid, szn))
		sleep(0.2)
	p1teammates.remove(p1id)

	p2teammates = set()
	for tid, szn in tqdm(p2teamsszns, desc=p2name):
		p2teammates.update(get_szn_teammates(tid, szn))
		sleep(0.2)
	p2teammates.remove(p2id)

	common = p1teammates.intersection(p2teammates)

	commoninfo = [players.find_player_by_id(i) for i in common]
	for p in commoninfo:
		print p['full_name']


# get_common_teammates('rajon rondo', 'chris paul')