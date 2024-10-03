//========= Copyright Valve Corporation, All rights reserved. ============//
//
// Purpose: Implements the bullsquid
//
// $NoKeywords: $
//=============================================================================//

#include "cbase.h"
#include "game.h"
#include "AI_Default.h"
#include "AI_Schedule.h"
#include "AI_Hull.h"
#include "AI_Navigator.h"
#include "AI_Motor.h"
#include "ai_squad.h"
#include "npc_bullsquid_new.h"
#include "npcevent.h"
#include "soundent.h"
#include "activitylist.h"
#include "weapon_brickbat.h"
#include "npc_headcrab.h"
#include "player.h"
#include "gamerules.h"		// For g_pGameRules
#include "ammodef.h"
#include "grenade_spit.h"
#include "grenade_brickbat.h"
#include "entitylist.h"
#include "shake.h"
#include "vstdlib/random.h"
#include "engine/IEngineSound.h"
#include "movevars_shared.h"
#include "particle_parse.h"
#include "gib.h"
#include "ai_interactions.h"

#include "AI_Hint.h"
#include "AI_Senses.h"

// memdbgon must be the last include file in a .cpp file!!!
#include "tier0/memdbgon.h"

#define		SQUID_SPRINT_DIST	256 // how close the squid has to get before starting to sprint and refusing to swerve

ConVar sk_bullsquid_health( "sk_bullsquid_health", "0" );
ConVar sk_bullsquid_dmg_bite( "sk_bullsquid_dmg_bite", "0" );
ConVar sk_bullsquid_dmg_whip( "sk_bullsquid_dmg_whip", "0" );
ConVar sk_bullsquid_spit_speed("sk_bullsquid_spit_speed", "0", FCVAR_NONE, "Speed at which a squid spit grenade travels.");
ConVar g_debug_squid( "g_debug_squid", "0" );

//=========================================================
// monster-specific schedule types
//=========================================================
enum
{
	SCHED_SQUID_HURTHOP = LAST_SHARED_SCHEDULE + 1,
	SCHED_SQUID_SEECRAB,
	SCHED_SQUID_EAT,
	SCHED_SQUID_SNIFF_AND_EAT,
	SCHED_SQUID_WALLOW,
	//SCHED_SQUID_SPIT_OUT_CLAWS,
};

//=========================================================
// monster-specific tasks
//=========================================================
enum 
{
	TASK_SQUID_HOPTURN = LAST_SHARED_TASK + 1,
	TASK_SQUID_EAT,
	//TASK_SQUID_SPIT_OUT_CLAWS,
};

//-----------------------------------------------------------------------------
// Squid Conditions
//-----------------------------------------------------------------------------
enum
{
	COND_SQUID_SMELL_FOOD	= LAST_SHARED_CONDITION + 1,
	//COND_SQUID_SPIT_OUT_CLAWS,
};


//=========================================================
// Interactions
//=========================================================
int	g_interactionBullsquidThrow		= 0;

//=========================================================
// Monster's Anim Events Go Here
//=========================================================
#define		BSQUID_AE_SPIT		( 1 )
#define		BSQUID_AE_BITE		( 2 )
#define		BSQUID_AE_BLINK		( 3 )
#define		BSQUID_AE_ROAR		( 4 )
#define		BSQUID_AE_HOP		( 5 )
#define		BSQUID_AE_THROW		( 6 )
#define		BSQUID_AE_WHIP_SND	( 7 )
#define		BSQUID_AE_TAILWHIP  ( 8 )

LINK_ENTITY_TO_CLASS( npc_bullsquid, CNPC_Bullsquid );

int ACT_SQUID_EXCITED;
int ACT_SQUID_EAT;
int ACT_SQUID_DETECT_SCENT;
int ACT_SQUID_INSPECT_FLOOR;
int ACT_SQUID_SPIT_OUT_CLAWS;

//int AE_SQUID_SPIT_OUT_CLAWS;

//---------------------------------------------------------
// Save/Restore
//---------------------------------------------------------
BEGIN_DATADESC( CNPC_Bullsquid )

	DEFINE_FIELD( m_fCanThreatDisplay,	FIELD_BOOLEAN ),
	DEFINE_FIELD( m_flLastHurtTime,		FIELD_TIME ),
	DEFINE_FIELD( m_flNextSpitTime,		FIELD_TIME ),
//	DEFINE_FIELD( m_nSquidSpitSprite,	FIELD_INTEGER ),
	DEFINE_FIELD( m_flHungryTime,		FIELD_TIME ),
	DEFINE_FIELD( m_nextSquidSoundTime,	FIELD_TIME ),
	DEFINE_FIELD( m_vecSaveSpitVelocity, FIELD_VECTOR ),

END_DATADESC()


//=========================================================
// Spawn
//=========================================================
void CNPC_Bullsquid::Spawn()
{
	Precache( );

	SetModel( "models/bullsquid.mdl");
	SetHullType(HULL_WIDE_SHORT);
	SetHullSizeNormal();

	SetSolid( SOLID_BBOX );
	//AddSolidFlags( FSOLID_NOT_STANDABLE );
	SetMoveType( MOVETYPE_STEP );
	m_bloodColor		= BLOOD_COLOR_GREEN;

	AddSpawnFlags(SF_NPC_LONG_RANGE);
	
	SetRenderColor( 255, 255, 255, 255 );
	
	m_iHealth			= sk_bullsquid_health.GetFloat();
	m_flFieldOfView		= 0.45;// indicates the width of this monster's forward view cone ( as a dotproduct result )
	m_NPCState			= NPC_STATE_IDLE;

	CapabilitiesClear();
	CapabilitiesAdd( bits_CAP_MOVE_GROUND | bits_CAP_INNATE_RANGE_ATTACK1 | bits_CAP_INNATE_MELEE_ATTACK1 | bits_CAP_INNATE_MELEE_ATTACK2 );
	
	m_fCanThreatDisplay	= TRUE;
	m_flNextSpitTime = gpGlobals->curtime;

	//m_flNextHuntTime = 0;
	//m_bCanCheckHunger = true;
	//m_HungerLevel = 0;

	//m_CrabTypeEaten = 0;

	NPCInit();

	//m_flDistTooFar		= 784;
}

//=========================================================
// Precache - precaches all resources this monster needs
//=========================================================
void CNPC_Bullsquid::Precache()
{
	PrecacheModel( "models/bullsquid.mdl" );
	PrecacheModel( "models/headcrab_gibs/classic_gib_leg1.mdl" );
	PrecacheModel( "models/headcrab_gibs/classic_gib_leg2.mdl" );
	PrecacheModel( "models/headcrab_gibs/classic_gib_leg3.mdl" );
	PrecacheModel( "models/headcrab_gibs/classic_gib_leg4.mdl" );

	//m_nSquidSpitSprite = PrecacheModel("sprites/greenspit1.vmt");// client side spittle.

	UTIL_PrecacheOther( "grenade_spit" );

	PrecacheParticleSystem("blood_impact_yellow_01");
	PrecacheParticleSystem("antlion_spit_player");

	PrecacheScriptSound( "NPC_Bullsquid.Idle" );
	PrecacheScriptSound( "NPC_Bullsquid.Pain" );
	PrecacheScriptSound( "NPC_Bullsquid.Alert" );
	PrecacheScriptSound( "NPC_Bullsquid.Death" );
	PrecacheScriptSound( "NPC_Bullsquid.Attack1" );
	PrecacheScriptSound( "NPC_Bullsquid.SpitAttack" );
	PrecacheScriptSound( "NPC_Bullsquid.Growl" );
	PrecacheScriptSound( "NPC_Bullsquid.Eat" );
	PrecacheScriptSound( "NPC_Bullsquid.TailWhip" );
	PrecacheScriptSound( "NPC_Bullsquid.FootstepLeft" );
	PrecacheScriptSound( "NPC_Bullsquid.FootstepRight" );
	PrecacheScriptSound( "NPC_Antlion.PoisonBall" );

	BaseClass::Precache();
}

//-----------------------------------------------------------------------------
// Purpose: Indicates this monster's place in the relationship table.
// Output : 
//-----------------------------------------------------------------------------
Class_T	CNPC_Bullsquid::Classify( void )
{
	return CLASS_BULLSQUID; 
}

//=========================================================
// IdleSound 
//=========================================================
#define SQUID_ATTN_IDLE	(float)1.5
void CNPC_Bullsquid::IdleSound( void )
{
	EmitSound( "NPC_Bullsquid.Idle" );
}

//=========================================================
// PainSound 
//=========================================================
void CNPC_Bullsquid::PainSound( const CTakeDamageInfo &info )
{
	EmitSound( "NPC_Bullsquid.Pain" );
}

//=========================================================
// AlertSound
//=========================================================
void CNPC_Bullsquid::AlertSound( void )
{
	EmitSound( "NPC_Bullsquid.Alert" );
}

//=========================================================
// DeathSound
//=========================================================
void CNPC_Bullsquid::DeathSound( const CTakeDamageInfo &info )
{
	EmitSound( "NPC_Bullsquid.Death" );
}

//=========================================================
// AttackSound
//=========================================================
void CNPC_Bullsquid::AttackSound( void )
{
	EmitSound( "NPC_Bullsquid.Attack1" );
}

//=========================================================
// SpitAttackSound
//=========================================================
void CNPC_Bullsquid::SpitAttackSound(void)
{
	EmitSound("NPC_Bullsquid.SpitAttack");
}

//=========================================================
// GrowlSound
//=========================================================
void CNPC_Bullsquid::GrowlSound( void )
{
	if (gpGlobals->curtime >= m_nextSquidSoundTime)
	{
		EmitSound( "NPC_Bullsquid.Growl" );
		m_nextSquidSoundTime	= gpGlobals->curtime + random->RandomInt(1.5,3.0);
	}
}


//=========================================================
// SetYawSpeed - allows each sequence to have a different
// turn rate associated with it.
//=========================================================
float CNPC_Bullsquid::MaxYawSpeed( void )
{
	float flYS = 0;

	switch ( GetActivity() )
	{
	case	ACT_WALK:			flYS = 90;	break;
	case	ACT_RUN:			flYS = 90;	break;
	case	ACT_IDLE:			flYS = 90;	break;
	case	ACT_RANGE_ATTACK1:	flYS = 90;	break;
	default:
		flYS = 90;
		break;
	}

	return flYS;
}

//=========================================================
// HandleAnimEvent - catches the monster-specific messages
// that occur when tagged animation frames are played.
//=========================================================
void CNPC_Bullsquid::HandleAnimEvent( animevent_t *pEvent )
{
	/*if (pEvent->event == AE_SQUID_SPIT_OUT_CLAWS)
	{
		CrabTypeEaten();
	}*/

	switch( pEvent->event )
	{
		case BSQUID_AE_SPIT:
		{
			if ( GetEnemy()->Classify() != CLASS_EARTH_FAUNA) //don't launch giant acid balls at birds, only eat them if they're next to you
			{
				Vector vSpitPos;
				GetAttachment("mouth", vSpitPos);
				//Vector			vTarget = GetEnemy()->GetAbsOrigin();

				/*Vector			vToss;
				CBaseEntity*	pBlocker;
				float flGravity  = SPIT_GRAVITY;
				ThrowLimit(vSpitPos, vTarget, flGravity, 3, Vector(0,0,0), Vector(0,0,0), GetEnemy(), &vToss, &pBlocker);

				CGrenadeSpit *pGrenade = (CGrenadeSpit*)CreateNoSpawn( "grenade_spit", vSpitPos, vec3_angle, this );
				//pGrenade->KeyValue( "velocity", vToss );
				pGrenade->Spawn( );
				pGrenade->SetThrower( this );
				pGrenade->SetOwnerEntity( this );
				pGrenade->SetSpitSize( 2 );
				pGrenade->SetAbsVelocity( vToss );

				// Tumble through the air
				pGrenade->SetLocalAngularVelocity(
					QAngle(
						random->RandomFloat( -100, -500 ),
						random->RandomFloat( -100, -500 ),
						random->RandomFloat( -100, -500 )
					)
				);
						
				AttackSound();
			
				CPVSFilter filter( vSpitPos );
				te->SpriteSpray( filter, 0.0,
					&vSpitPos, &vToss, m_nSquidSpitSprite, 5, 10, 15 );*/

				//Vector vSpitPos;
				//GetAttachment("mouth", vSpitPos);

				Vector	vTarget;

				// If our enemy is looking at us and far enough away, lead him
				if (HasCondition(COND_ENEMY_FACING_ME) && UTIL_DistApprox(GetAbsOrigin(), GetEnemy()->GetAbsOrigin()) > (40 * 12))
				{
					UTIL_PredictedPosition(GetEnemy(), 0.5f, &vTarget);
					vTarget.z = GetEnemy()->GetAbsOrigin().z;
				}
				else
				{
					// Otherwise he can't see us and he won't be able to dodge
					vTarget = GetEnemy()->BodyTarget(vSpitPos, true);
				}

				vTarget[2] += random->RandomFloat(0.0f, 32.0f);

				// Try and spit at our target
				Vector	vecToss;
				if (GetSpitVector(vSpitPos, vTarget, &vecToss) == false)
				{
					// Now try where they were
					if (GetSpitVector(vSpitPos, m_vSavePosition, &vecToss) == false)
					{
						// Failing that, just shoot with the old velocity we calculated initially!
						vecToss = m_vecSaveSpitVelocity;
					}
				}

				// Find what our vertical theta is to estimate the time we'll impact the ground
				Vector vecToTarget = (vTarget - vSpitPos);
				VectorNormalize(vecToTarget);
				float flVelocity = VectorNormalize(vecToss);
				float flCosTheta = DotProduct(vecToTarget, vecToss);
				float flTime = (vSpitPos - vTarget).Length2D() / (flVelocity * flCosTheta);

				// Emit a sound where this is going to hit so that targets get a chance to act correctly
				CSoundEnt::InsertSound(SOUND_DANGER, vTarget, (15 * 12), flTime, this);

				// Don't fire again until this volley would have hit the ground (with some lag behind it)
				SetNextAttack(gpGlobals->curtime + flTime + random->RandomFloat(0.5f, 2.0f));

				// Tell any squadmates not to fire for some portion of the time this volley will be in the air (except on hard)
				//if (g_pGameRules->IsSkillLevel(SKILL_HARD) == false)
				//	DelaySquadAttack(flTime);

				for (int i = 0; i < 6; i++)
				{
					CGrenadeSpit *pGrenade = (CGrenadeSpit*)CreateEntityByName("grenade_spit");
					pGrenade->SetAbsOrigin(vSpitPos);
					pGrenade->SetAbsAngles(vec3_angle);
					DispatchSpawn(pGrenade);
					pGrenade->SetThrower(this);
					pGrenade->SetOwnerEntity(this);

					if (i == 0)
					{
						pGrenade->SetSpitSize(SPIT_LARGE);
						pGrenade->SetAbsVelocity(vecToss * flVelocity);
					}
					else
					{
						pGrenade->SetAbsVelocity((vecToss + RandomVector(-0.035f, 0.035f)) * flVelocity);
						pGrenade->SetSpitSize(random->RandomInt(SPIT_SMALL, SPIT_MEDIUM));
					}

					// Tumble through the air
					pGrenade->SetLocalAngularVelocity(
						QAngle(random->RandomFloat(-250, -500),
						random->RandomFloat(-250, -500),
						random->RandomFloat(-250, -500)));
				}

				/*for (int i = 0; i < 8; i++)
				{
					DispatchParticleEffect("blood_impact_yellow_01", vSpitPos + RandomVector(-12.0f, 12.0f), RandomAngle(0, 360));
				}*/

				SpitAttackSound();
			}
		}
		break;

		case BSQUID_AE_BITE:
		{
		// SOUND HERE!

			
			//Headcrab chow time!
			if (GetEnemy()->Classify() == CLASS_HEADCRAB)
			{
				CBaseEntity *pHurt = CheckTraceHullAttack(70, Vector(-16, -16, -16), Vector(16, 16, 16), GetEnemy()->m_iHealth, DMG_REMOVENORAGDOLL);
				if (pHurt)
				{
					Vector vecMouth;
					GetAttachment("mouth", vecMouth);
					QAngle angles;
					Vector vecFoodPos = pHurt->WorldSpaceCenter();
					Vector vecFoodDelta = vecFoodPos - vecMouth;

					m_RandomHCGib = random->RandomInt(1, 5);
					DispatchParticleEffect("antlion_spit_player", vecMouth, angles);
					EmitSound("NPC_Bullsquid.Eat");
					UTIL_BloodSpray(vecMouth, vecFoodDelta, BLOOD_COLOR_GREEN, 7, FX_BLOODSPRAY_ALL);
					m_flHungryTime = gpGlobals->curtime + random->RandomInt(5, 18);

					if (FClassnameIs(GetEnemy(), "npc_headcrab"))
					{
						//m_CrabTypeEaten = 1;
						//SetSchedule(SCHED_SQUID_SPIT_OUT_CLAWS);
						//m_HungerLevel = m_HungerLevel + 5;
						if (m_RandomHCGib == 1)
						{
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg1.mdl", 5);
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg2.mdl", 5);
						}
						if (m_RandomHCGib == 2)
						{
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg1.mdl", 5);
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg3.mdl", 5);
						}
						if (m_RandomHCGib == 3)
						{
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg2.mdl", 5);
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg4.mdl", 5);
						}
						if (m_RandomHCGib == 4)
						{
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg1.mdl", 5);
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg2.mdl", 5);
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg3.mdl", 5);
						}
						if (m_RandomHCGib == 5)
						{
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg1.mdl", 5);
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg2.mdl", 5);
							CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg4.mdl", 5);
						}
					}
					else if (FClassnameIs(GetEnemy(), "npc_headcrab_fast"))
					{
						//m_CrabTypeEaten = 2;
						//SetSchedule(SCHED_SQUID_SPIT_OUT_CLAWS);
						//m_HungerLevel = m_HungerLevel + 4;

					}
					else if (FClassnameIs(GetEnemy(), "npc_headcrab_poison"))
					{
						//m_CrabTypeEaten = 3;
						//SetSchedule(SCHED_SQUID_SPIT_OUT_CLAWS);
						//m_HungerLevel = m_HungerLevel + 6;

					}
				}
			}
			else
			{
				CBaseEntity *pHurt = CheckTraceHullAttack(70, Vector(-16, -16, -16), Vector(16, 16, 16), sk_bullsquid_dmg_bite.GetFloat(), DMG_SLASH);
				if (pHurt)
				{
					Vector forward, up;
					AngleVectors(GetAbsAngles(), &forward, NULL, &up);
					pHurt->ApplyAbsVelocityImpulse(100 * (up - forward));
					pHurt->SetGroundEntity(NULL);
				}
			}
			
		}
		break;

		case BSQUID_AE_WHIP_SND:
		{
			EmitSound( "NPC_Bullsquid.TailWhip" );
			break;
		}


		case BSQUID_AE_TAILWHIP:
		{
			float flDamage = (GetEnemy()->Classify() == CLASS_ZOMBIE) ? GetEnemy()->m_iHealth : sk_bullsquid_dmg_whip.GetFloat();

			CBaseEntity *pHurt = CheckTraceHullAttack( 70, Vector(-16,-16,-16), Vector(16,16,16), flDamage, DMG_SLASH );
			
			if ( pHurt ) 
			{
				Vector right, up;
				AngleVectors( GetAbsAngles(), NULL, &right, &up );

				if ( pHurt->GetFlags() & ( FL_NPC | FL_CLIENT ) )
					 pHurt->ViewPunch( QAngle( 20, 0, -20 ) );

				pHurt->ApplyAbsVelocityImpulse( 100 * (up+2*right) );
			}
		}
		break;


		case BSQUID_AE_BLINK:
		{
			// close eye. 
			m_nSkin = 1;
		}
		break;

		case BSQUID_AE_HOP:
		{
			float flGravity = GetCurrentGravity();

			// throw the squid up into the air on this frame.
			if ( GetFlags() & FL_ONGROUND )
			{
				SetGroundEntity( NULL );
			}

			// jump 40 inches into the air
			Vector vecVel = GetAbsVelocity();
			vecVel.z += sqrt( flGravity * 2.0 * 40 );
			SetAbsVelocity( vecVel );
		}
		break;

		case BSQUID_AE_THROW:
			{
				// squid throws its prey IF the prey is a client. 
				CBaseEntity *pHurt = CheckTraceHullAttack( 70, Vector(-16,-16,-16), Vector(16,16,16), 0, 0 );


				if ( pHurt )
				{
					pHurt->ViewPunch( QAngle(20,0,-20) );
							
					// screeshake transforms the viewmodel as well as the viewangle. No problems with seeing the ends of the viewmodels.
					UTIL_ScreenShake( pHurt->GetAbsOrigin(), 25.0, 1.5, 0.7, 2, SHAKE_START );

					// If the player, throw him around
					if ( pHurt->IsPlayer())
					{
						Vector forward, up;
						AngleVectors( GetLocalAngles(), &forward, NULL, &up );
						pHurt->ApplyAbsVelocityImpulse( forward * 300 + up * 300 );
					}
					// If not the player see if has bullsquid throw interatcion
					else
					{
						CBaseCombatCharacter *pVictim = ToBaseCombatCharacter( pHurt );
						if (pVictim)
						{
							if ( pVictim->DispatchInteraction( g_interactionBullsquidThrow, NULL, this ) )
							{
								Vector forward, up;
								AngleVectors( GetLocalAngles(), &forward, NULL, &up );
								pVictim->ApplyAbsVelocityImpulse( forward * 300 + up * 250 );
							}
						}
					}
				}
			}
		break;

		case NPC_EVENT_LEFTFOOT:
		{
			EmitSound("NPC_Bullsquid.FootstepLeft", pEvent->eventtime);
		}
		break;

		case NPC_EVENT_RIGHTFOOT:
		{
			EmitSound("NPC_Bullsquid.FootstepRight", pEvent->eventtime);
		}
		break;


		default:
			BaseClass::HandleAnimEvent( pEvent );
	}
}

//-----------------------------------------------------------------------------
// Purpose:  This is a generic function (to be implemented by sub-classes) to
//			 handle specific interactions between different types of characters
//			 (For example the barnacle grabbing an NPC)
// Input  :  Constant for the type of interaction
// Output :	 true  - if sub-class has a response for the interaction
//			 false - if sub-class has no response
//-----------------------------------------------------------------------------
bool CNPC_Bullsquid::HandleInteraction(int interactionType, void *data, CBaseCombatCharacter* sourceEnt)
{
	if (interactionType == g_interactionBarnacleVictimDangle)
	{
		// Die instantly
		return false;
	}

	return BaseClass::HandleInteraction(interactionType, data, sourceEnt);
}

int CNPC_Bullsquid::RangeAttack1Conditions( float flDot, float flDist )
{
	if ( IsMoving() && flDist >= 512 )
	{
		// squid will far too far behind if he stops running to spit at this distance from the enemy.
		return ( COND_NONE );
	}

	if (flDist > 85 && flDist <= 784 && flDot >= 0.5 && gpGlobals->curtime >= m_flNextSpitTime && !FClassnameIs(GetEnemy(), "npc_headcrab") && !FClassnameIs(GetEnemy(), "npc_headcrab_fast") && !FClassnameIs(GetEnemy(), "npc_headcrab_black") && (GetEnemy()->Classify() != CLASS_ZOMBIE))
	{
		if ( GetEnemy() != NULL )
		{
			if ( fabs( GetAbsOrigin().z - GetEnemy()->GetAbsOrigin().z ) > 256 )
			{
				// don't try to spit at someone up really high or down really low.
				return( COND_NONE );
			}
		}

		if ( IsMoving() )
		{
			// don't spit again for a long time, resume chasing enemy.
			m_flNextSpitTime = gpGlobals->curtime + 2.5;
		}
		else
		{
			// not moving, so spit again pretty soon.
			m_flNextSpitTime = gpGlobals->curtime + 0.5;
		}

		return( COND_CAN_RANGE_ATTACK1 );
	}

	return( COND_NONE );
}

//-----------------------------------------------------------------------------
// Purpose: 
//-----------------------------------------------------------------------------
/*int CNPC_Bullsquid::RangeAttack1Conditions(float flDot, float flDist)
{
	if (GetNextAttack() > gpGlobals->curtime)
		return COND_NOT_FACING_ATTACK;

	if (flDot < DOT_10DEGREE)
		return COND_NOT_FACING_ATTACK;

	if (flDist >(300 * 12)) //was 150 * 12
		return COND_TOO_FAR_TO_ATTACK;

	if (flDist < (20 * 12))
		return COND_TOO_CLOSE_TO_ATTACK;

	return COND_CAN_RANGE_ATTACK1;
}*/

//=========================================================
// MeleeAttack2Conditions - bullsquid is a big guy, so has a longer
// melee range than most monsters. This is the tailwhip attack
//=========================================================
int CNPC_Bullsquid::MeleeAttack1Conditions( float flDot, float flDist )
{
	
	if ( flDist <= 85 && flDot >= 0.7 && (GetEnemy()->Classify() == CLASS_ZOMBIE))
	{
		return ( COND_CAN_MELEE_ATTACK1 );
	}

	else if (GetEnemy()->m_iHealth <= sk_bullsquid_dmg_whip.GetFloat() && flDist <= 85 && flDot >= 0.7 && !FClassnameIs(GetEnemy(), "npc_headcrab") && !FClassnameIs(GetEnemy(), "npc_headcrab_fast") && !FClassnameIs(GetEnemy(), "npc_headcrab_black"))
	{
		return ( COND_CAN_MELEE_ATTACK1 );
	}
	
	return( COND_NONE );
}

//=========================================================
// MeleeAttack2Conditions - bullsquid is a big guy, so has a longer
// melee range than most monsters. This is the bite attack.
// this attack will not be performed if the tailwhip attack
// is valid.
//=========================================================
int CNPC_Bullsquid::MeleeAttack2Conditions( float flDot, float flDist )
{
	if (flDist <= 85 && flDot >= 0.7 && !HasCondition(COND_CAN_MELEE_ATTACK1) && (GetEnemy()->Classify() != CLASS_ZOMBIE))		// The player & bullsquid can be as much as their bboxes 
		 return ( COND_CAN_MELEE_ATTACK2 );
	
	return( COND_NONE );
}

bool CNPC_Bullsquid::FValidateHintType( CAI_Hint *pHint )
{
	if ( pHint->HintType() == HINT_HL1_WORLD_HUMAN_BLOOD )
		 return true;

	DevMsg( "Couldn't validate hint type" );

	return false;
}

void CNPC_Bullsquid::RemoveIgnoredConditions( void )
{
	if ( m_flHungryTime > gpGlobals->curtime )
		 ClearCondition( COND_SQUID_SMELL_FOOD );

	if ( gpGlobals->curtime - m_flLastHurtTime <= 20 )
	{
		// haven't been hurt in 20 seconds, so let the squid care about stink. 
		ClearCondition( COND_SMELL );
	}

	if ( GetEnemy() != NULL )
	{
		// ( Unless after a tasty headcrab, yumm ^_^ )
		if ( FClassnameIs( GetEnemy(), "npc_headcrab" ) )
			 ClearCondition( COND_SMELL );
	}
}

Disposition_t CNPC_Bullsquid::IRelationType( CBaseEntity *pTarget )
{
	if ( gpGlobals->curtime - m_flLastHurtTime < 5 && FClassnameIs( pTarget, "npc_headcrab" ) )
	{
		// if squid has been hurt in the last 5 seconds, and is getting relationship for a headcrab, 
		// tell squid to disregard crab. 
		return D_NU;
	}

	/*if (m_flNextHuntTime > 0)
	{
		AddClassRelationship( CLASS_HEADCRAB, D_NU, 0 );
		AddClassRelationship( CLASS_ZOMBIE, D_NU, 0 );
	}
	else if (m_flNextHuntTime == 0)
	{
		AddClassRelationship(CLASS_HEADCRAB, D_HT, 0);
		AddClassRelationship(CLASS_ZOMBIE, D_HT, 0);
	}*/

	return BaseClass::IRelationType( pTarget );
}
//=========================================================
// Purpose: 
// 
//=========================================================
/*int CNPC_Bullsquid::SpitOutClawConditions( void )
{
	if (m_CrabTypeEaten != 0)
	{
		return COND_SQUID_SPIT_OUT_CLAWS;
	}
	return COND_NONE;
}*/
//=========================================================
// Purpose: 
// 
//=========================================================
/*void CNPC_Bullsquid::CrabTypeEaten()
{
	m_RandomHCGib = random->RandomInt(1, 5);
	DevMsg("gib event is activated.\n");
	if (m_CrabTypeEaten == 1)
	{
		m_CrabTypeEaten = 0;
		if (m_RandomHCGib == 1)
		{
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg1.mdl", 5);
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg2.mdl", 5);
		}
		if (m_RandomHCGib == 2)
		{
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg1.mdl", 5);
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg3.mdl", 5);
		}
		if (m_RandomHCGib == 3)
		{
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg2.mdl", 5);
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg4.mdl", 5);
		}
		if (m_RandomHCGib == 4)
		{
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg1.mdl", 5);
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg2.mdl", 5);
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg3.mdl", 5);
		}
		if (m_RandomHCGib == 5)
		{
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg1.mdl", 5);
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg2.mdl", 5);
			CGib::SpawnAngularGibs(pHurt, angles, 1, 450, 200, "models/headcrab_gibs/classic_gib_leg4.mdl", 5);
		}
	}
	if (m_CrabTypeEaten == 2)
	{
		m_CrabTypeEaten = 0;

	}
	if (m_CrabTypeEaten == 3)
	{
		m_CrabTypeEaten = 0;

	}
}*/

//=========================================================
// TakeDamage - overridden for bullsquid so we can keep track
// of how much time has passed since it was last injured
//=========================================================
int CNPC_Bullsquid::OnTakeDamage_Alive( const CTakeDamageInfo &inputInfo )
{

#if 0 //Fix later.

	float flDist;
	Vector vecApex, vOffset;

	// if the squid is running, has an enemy, was hurt by the enemy, hasn't been hurt in the last 3 seconds, and isn't too close to the enemy,
	// it will swerve. (whew).
	if ( GetEnemy() != NULL && IsMoving() && pevAttacker == GetEnemy() && gpGlobals->curtime - m_flLastHurtTime > 3 )
	{
		flDist = ( GetAbsOrigin() - GetEnemy()->GetAbsOrigin() ).Length2D();
		
		if ( flDist > SQUID_SPRINT_DIST )
		{
			AI_Waypoint_t*	pRoute = GetNavigator()->GetPath()->Route();

			if ( pRoute )
			{
				flDist = ( GetAbsOrigin() - pRoute[ pRoute->iNodeID ].vecLocation ).Length2D();// reusing flDist. 

				if ( GetNavigator()->GetPath()->BuildTriangulationRoute( GetAbsOrigin(), pRoute[ pRoute->iNodeID ].vecLocation, flDist * 0.5, GetEnemy(), &vecApex, &vOffset, NAV_GROUND ) )
				{
					GetNavigator()->PrependWaypoint( vecApex, bits_WP_TO_DETOUR | bits_WP_DONT_SIMPLIFY );
				}
			}
		}
	}
#endif
	if ((GetDamageType() & (DMG_ACID | DMG_POISON)) || (inputInfo.GetAttacker()->Classify() == CLASS_BULLSQUID))
		return 0;

	return BaseClass::OnTakeDamage_Alive( inputInfo );
}

//=========================================================
// GetSoundInterests - returns a bit mask indicating which types
// of sounds this monster regards. In the base class implementation,
// monsters care about all sounds, but no scents.
//=========================================================
int CNPC_Bullsquid::GetSoundInterests( void )
{
	return	SOUND_WORLD	|
			SOUND_COMBAT	|
		    SOUND_CARCASS	|
			SOUND_MEAT		|
			SOUND_GARBAGE	|
			SOUND_PLAYER;
}

//=========================================================
// OnListened - monsters dig through the active sound list for
// any sounds that may interest them. (smells, too!)
//=========================================================
void CNPC_Bullsquid::OnListened( void )
{
	AISoundIter_t iter;
	
	CSound *pCurrentSound;

	static int conditionsToClear[] = 
	{
		COND_SQUID_SMELL_FOOD,
	};

	ClearConditions( conditionsToClear, ARRAYSIZE( conditionsToClear ) );
	
	pCurrentSound = GetSenses()->GetFirstHeardSound( &iter );
	
	while ( pCurrentSound )
	{
		// the npc cares about this sound, and it's close enough to hear.
		int condition = COND_NONE;
		
		if ( !pCurrentSound->FIsSound() )
		{
			// if not a sound, must be a smell - determine if it's just a scent, or if it's a food scent
			if ( pCurrentSound->m_iType & ( SOUND_MEAT | SOUND_CARCASS ) )
			{
				// the detected scent is a food item
				condition = COND_SQUID_SMELL_FOOD;
			}
		}
		
		if ( condition != COND_NONE )
			SetCondition( condition );

		pCurrentSound = GetSenses()->GetNextHeardSound( &iter );
	}

	BaseClass::OnListened();
}

//========================================================
// RunAI - overridden for bullsquid because there are things
// that need to be checked every think.
//========================================================
void CNPC_Bullsquid::RunAI( void )
{
	// first, do base class stuff
	BaseClass::RunAI();

	if ( m_nSkin != 0 )
	{
		// close eye if it was open.
		m_nSkin = 0; 
	}

	if ( random->RandomInt( 0,39 ) == 0 )
	{
		m_nSkin = 1;
	}

	if ( GetEnemy() != NULL && GetActivity() == ACT_RUN )
	{
		// chasing enemy. Sprint for last bit
		if ( (GetAbsOrigin() - GetEnemy()->GetAbsOrigin()).Length2D() < SQUID_SPRINT_DIST )
		{
			m_flPlaybackRate = 1.25;
		}
	}

	//SpitOutClawConditions();


	if (gpGlobals->curtime >= m_flHungryTime)
	{
		AddClassRelationship(CLASS_HEADCRAB, D_HT, 0);
		AddClassRelationship(CLASS_ZOMBIE, D_HT, 0);
	}
	else
	{
		AddClassRelationship(CLASS_HEADCRAB, D_NU, 0);
		AddClassRelationship(CLASS_ZOMBIE, D_NU, 0);
	}
}

//=========================================================
// GetSchedule 
//=========================================================
int CNPC_Bullsquid::SelectSchedule( void )
{
	switch	( m_NPCState )
	{
	case NPC_STATE_ALERT:
		{
			/*if ( HasCondition( COND_LIGHT_DAMAGE ) || HasCondition( COND_HEAVY_DAMAGE ) )
			{
				return SCHED_SQUID_HURTHOP;
			}*/

			if ( HasCondition( COND_SQUID_SMELL_FOOD ) )
			{
				CSound		*pSound;

				pSound = GetBestScent();
				
				if ( pSound && (!FInViewCone( pSound->GetSoundOrigin() ) || !FVisible( pSound->GetSoundOrigin() )) )
				{
					// scent is behind or occluded
					return SCHED_SQUID_SNIFF_AND_EAT;
				}

				// food is right out in the open. Just go get it.
				return SCHED_SQUID_EAT;
			}

			if ( HasCondition( COND_SMELL ) )
			{
				// there's something stinky. 
				CSound		*pSound;

				pSound = GetBestScent();
				if ( pSound )
					return SCHED_SQUID_WALLOW;
			}

			break;
		}
	case NPC_STATE_COMBAT:
		{
// dead enemy
			if ( HasCondition( COND_ENEMY_DEAD ) )
			{
				// call base class, all code to handle dead enemies is centralized there.
				return BaseClass::SelectSchedule();
			}

			if ( HasCondition( COND_NEW_ENEMY ) )
			{
				/*if ( m_fCanThreatDisplay && IRelationType( GetEnemy() ) == D_HT && FClassnameIs( GetEnemy(), "npc_headcrab" ) )
				{
					// this means squid sees a headcrab!
					m_fCanThreatDisplay = FALSE;// only do the headcrab dance once per lifetime.
					return SCHED_SQUID_SEECRAB;
				}
				else
				{*/
					return SCHED_WAKE_ANGRY;
				//}
			}

			if ( HasCondition( COND_SQUID_SMELL_FOOD ) )
			{
				CSound		*pSound;

				pSound = GetBestScent();
				
				if ( pSound && (!FInViewCone( pSound->GetSoundOrigin() ) || !FVisible( pSound->GetSoundOrigin() )) )
				{
					// scent is behind or occluded
					return SCHED_SQUID_SNIFF_AND_EAT;
				}

				// food is right out in the open. Just go get it.
				return SCHED_SQUID_EAT;
			}

			// Range attack if we're able
			if (HasCondition(COND_CAN_RANGE_ATTACK1))
			{
				return SCHED_RANGE_ATTACK1;
			}

			// Back up, we're too near an enemy or can't see them
			if (HasCondition(COND_TOO_CLOSE_TO_ATTACK) || HasCondition(COND_ENEMY_OCCLUDED))
				return SCHED_ESTABLISH_LINE_OF_FIRE;

			// See if we need to destroy breakable cover
			if (HasCondition(COND_WEAPON_SIGHT_OCCLUDED))
				return SCHED_SHOOT_ENEMY_COVER;

			if ( HasCondition( COND_CAN_MELEE_ATTACK1 ) )
			{
				return SCHED_MELEE_ATTACK1;
			}

			if ( HasCondition( COND_CAN_MELEE_ATTACK2 ) )
			{
				return SCHED_MELEE_ATTACK2;
			}
			
			return SCHED_CHASE_ENEMY;

			break;
		}
	}

	/*if (HasCondition(COND_SQUID_SPIT_OUT_CLAWS))
	{
		return SCHED_SQUID_SPIT_OUT_CLAWS;
	}*/

	return BaseClass::SelectSchedule();
}

//=========================================================
// FInViewCone - returns true is the passed vector is in
// the caller's forward view cone. The dot product is performed
// in 2d, making the view cone infinitely tall. 
//=========================================================
bool CNPC_Bullsquid::FInViewCone( Vector pOrigin )
{
	Vector los = ( pOrigin - GetAbsOrigin() );

	// do this in 2D
	los.z = 0;
	VectorNormalize( los );

	Vector facingDir = EyeDirection2D( );

	float flDot = DotProduct( los, facingDir );

	if ( flDot > m_flFieldOfView )
		return true;

	return false;
}

//=========================================================
// Start task - selects the correct activity and performs
// any necessary calculations to start the next task on the
// schedule.  OVERRIDDEN for bullsquid because it needs to
// know explicitly when the last attempt to chase the enemy
// failed, since that impacts its attack choices.
//=========================================================
void CNPC_Bullsquid::StartTask( const Task_t *pTask )
{
	switch ( pTask->iTask )
	{
	/*case TASK_MELEE_ATTACK1:
	{
		if ((GetEnemy()->Classify() == CLASS_ZOMBIE))
		{
			

			m_flLastAttackTime = gpGlobals->curtime;

			BaseClass::StartTask(pTask);
		}
		break;
	}*/
	/*case TASK_SQUID_SPIT_OUT_CLAWS:
		{
			SetIdealActivity(ACT_SQUID_SPIT_OUT_CLAWS);
			break;
		}*/

	case TASK_MELEE_ATTACK2:
		{
			if (GetEnemy())
			{
				GrowlSound();

				m_flLastAttackTime = gpGlobals->curtime;

				BaseClass::StartTask( pTask );
			}
			break;
		}
	case TASK_SQUID_HOPTURN:
		{
			SetActivity( ACT_HOP );
			
			if ( GetEnemy() )
			{
				Vector	vecFacing = ( GetEnemy()->GetAbsOrigin() - GetAbsOrigin() );
				VectorNormalize( vecFacing );

				GetMotor()->SetIdealYaw( vecFacing );
			}

			break;
		}
	case TASK_SQUID_EAT:
		{
			m_flHungryTime = gpGlobals->curtime + pTask->flTaskData;
			TaskComplete();
			break;
		}
	default:
		{
			BaseClass::StartTask( pTask );
			break;
		}
	}
}

//=========================================================
// RunTask
//=========================================================
void CNPC_Bullsquid::RunTask( const Task_t *pTask )
{
	switch ( pTask->iTask )
	{
	case TASK_SQUID_HOPTURN:
		{
			if ( GetEnemy() )
			{
				Vector	vecFacing = ( GetEnemy()->GetAbsOrigin() - GetAbsOrigin() );
				VectorNormalize( vecFacing );
				GetMotor()->SetIdealYaw( vecFacing );
			}

			if ( IsSequenceFinished() )
			{
				TaskComplete(); 
			}
			break;
		}
	default:
		{
			BaseClass::RunTask( pTask );
			break;
		}
	}
}

//=========================================================
// GetIdealState - Overridden for Bullsquid to deal with
// the feature that makes it lose interest in headcrabs for 
// a while if something injures it. 
//=========================================================
NPC_STATE CNPC_Bullsquid::SelectIdealState( void )
{
	// If no schedule conditions, the new ideal state is probably the reason we're in here.
	/*switch ( m_NPCState )
	{
		case NPC_STATE_COMBAT:
		{
			// COMBAT goes to ALERT upon death of enemy
			if ( GetEnemy() != NULL && ( HasCondition( COND_LIGHT_DAMAGE ) || HasCondition( COND_HEAVY_DAMAGE ) ) && FClassnameIs( GetEnemy(), "npc_headcrab" ) )
			{
				// if the squid has a headcrab enemy and something hurts it, it's going to forget about the crab for a while.
				SetEnemy( NULL );
				return NPC_STATE_ALERT;
			}
			break;
		}
	}*/

	return BaseClass::SelectIdealState();
}

//=========================================================
// NPC Think
//=========================================================
/*void CNPC_Bullsquid::NPCThink()
{
	BaseClass::NPCThink();

	//Bullsquid Headcrab hunting
	SquidHungerLevel();

	if (m_flNextHuntTime > 0)
	{
		DevMsg("Bullsquid is not hungry for %s seconds!\n", m_flNextHuntTime);
	}
	if (m_flNextHuntTime == 0)
	{
		m_bCanCheckHunger = true;
	}
}*/

//-----------------------------------------------------------------------------
// Purpose: Returns whether the enemy has been seen within the time period supplied
// Input  : flTime - Timespan we consider
// Output : Returns true on success, false on failure.
//-----------------------------------------------------------------------------
bool CNPC_Bullsquid::SeenSquidEnemyWithinTime(float flTime)
{
	float flLastSeenTime = GetEnemies()->LastTimeSeen(GetEnemy());
	return (flLastSeenTime != 0.0f && (gpGlobals->curtime - flLastSeenTime) < flTime);
}

//-----------------------------------------------------------------------------
// Purpose: Test whether this bullsquid can hit the target
//-----------------------------------------------------------------------------
bool CNPC_Bullsquid::InnateWeaponLOSCondition(const Vector &ownerPos, const Vector &targetPos, bool bSetConditions)
{
	if (GetNextAttack() > gpGlobals->curtime)
		return false;

	// If we can see the enemy, or we've seen them in the last few seconds just try to lob in there
	if (SeenSquidEnemyWithinTime(3.0f))
	{
		Vector vSpitPos;
		GetAttachment("mouth", vSpitPos);

		return GetSpitVector(vSpitPos, targetPos, &m_vecSaveSpitVelocity);
	}

	return BaseClass::InnateWeaponLOSCondition(ownerPos, targetPos, bSetConditions);
}

//
//	FIXME: Create this in a better fashion!
//

Vector VecCheckSquidSpitTolerance(CBaseEntity *pEdict, const Vector &vecSpot1, Vector vecSpot2, float flSpeed, float flTolerance)
{
	flSpeed = MAX(1.0f, flSpeed);

	float flGravity = GetCurrentGravity();

	Vector vecGrenadeVel = (vecSpot2 - vecSpot1);

	// throw at a constant time
	float time = vecGrenadeVel.Length() / flSpeed;
	vecGrenadeVel = vecGrenadeVel * (1.0 / time);

	// adjust upward toss to compensate for gravity loss
	vecGrenadeVel.z += flGravity * time * 0.5;

	Vector vecApex = vecSpot1 + (vecSpot2 - vecSpot1) * 0.5;
	vecApex.z += 0.5 * flGravity * (time * 0.5) * (time * 0.5);


	trace_t tr;
	UTIL_TraceLine(vecSpot1, vecApex, MASK_SOLID, pEdict, COLLISION_GROUP_NONE, &tr);
	if (tr.fraction != 1.0)
	{
		// fail!
		if (g_debug_squid.GetBool())
		{
			NDebugOverlay::Line(vecSpot1, vecApex, 255, 0, 0, true, 5.0);
		}

		return vec3_origin;
	}

	if (g_debug_squid.GetBool())
	{
		NDebugOverlay::Line(vecSpot1, vecApex, 0, 255, 0, true, 5.0);
	}

	UTIL_TraceLine(vecApex, vecSpot2, MASK_SOLID_BRUSHONLY, pEdict, COLLISION_GROUP_NONE, &tr);
	if (tr.fraction != 1.0)
	{
		bool bFail = true;

		// Didn't make it all the way there, but check if we're within our tolerance range
		if (flTolerance > 0.0f)
		{
			float flNearness = (tr.endpos - vecSpot2).LengthSqr();
			if (flNearness < Square(flTolerance))
			{
				if (g_debug_squid.GetBool())
				{
					NDebugOverlay::Sphere(tr.endpos, vec3_angle, flTolerance, 0, 255, 0, 0, true, 5.0);
				}

				bFail = false;
			}
		}

		if (bFail)
		{
			if (g_debug_squid.GetBool())
			{
				NDebugOverlay::Line(vecApex, vecSpot2, 255, 0, 0, true, 5.0);
				NDebugOverlay::Sphere(tr.endpos, vec3_angle, flTolerance, 255, 0, 0, 0, true, 5.0);
			}
			return vec3_origin;
		}
	}

	if (g_debug_squid.GetBool())
	{
		NDebugOverlay::Line(vecApex, vecSpot2, 0, 255, 0, true, 5.0);
	}

	return vecGrenadeVel;
}

//-----------------------------------------------------------------------------
// Purpose: Get a toss direction that will properly lob spit to hit a target
// Input  : &vecStartPos - Where the spit will start from
//			&vecTarget - Where the spit is meant to land
//			*vecOut - The resulting vector to lob the spit
// Output : Returns true on success, false on failure.
//-----------------------------------------------------------------------------
bool CNPC_Bullsquid::GetSpitVector(const Vector &vecStartPos, const Vector &vecTarget, Vector *vecOut)
{
	// Try the most direct route
	Vector vecToss = VecCheckSquidSpitTolerance(this, vecStartPos, vecTarget, sk_bullsquid_spit_speed.GetFloat(), (10.0f*12.0f));

	// If this failed then try a little faster (flattens the arc)
	if (vecToss == vec3_origin)
	{
		vecToss = VecCheckSquidSpitTolerance(this, vecStartPos, vecTarget, sk_bullsquid_spit_speed.GetFloat() * 1.5f, (10.0f*12.0f));
		if (vecToss == vec3_origin)
			return false;
		//DevMsg("Spit Vector Failed.\n");
	}

	// Save out the result
	if (vecOut)
	{
		*vecOut = vecToss;
	}

	return true;
}

//-----------------------------------------------------------------------------
// Purpose: Calculate how full this bullsquid is.
//-----------------------------------------------------------------------------
/*void CNPC_Bullsquid::SquidHungerLevel()
{
	if (m_bCanCheckHunger == true)
	{
		if (m_HungerLevel >= 7)
		{
			m_bCanCheckHunger = false;
			m_flNextHuntTime = gpGlobals->curtime + 15;
			if (m_HungerLevel > 7)
			{
				m_flNextHuntTime = gpGlobals->curtime + (m_HungerLevel - 7);
				m_HungerLevel = 0;

			}
			else
			{
				m_HungerLevel = 0;
			}
		}
		else
		{
			DevMsg("Bullsquid is now hungry again!\n");
		}
	}
}*/

//------------------------------------------------------------------------------
//
// Schedules
//
//------------------------------------------------------------------------------

AI_BEGIN_CUSTOM_NPC( npc_bullsquid, CNPC_Bullsquid )

	DECLARE_TASK( TASK_SQUID_HOPTURN )
	DECLARE_TASK( TASK_SQUID_EAT )

	DECLARE_CONDITION( COND_SQUID_SMELL_FOOD )

	DECLARE_ACTIVITY( ACT_SQUID_EXCITED )
	DECLARE_ACTIVITY( ACT_SQUID_EAT )
	DECLARE_ACTIVITY( ACT_SQUID_DETECT_SCENT )
	DECLARE_ACTIVITY( ACT_SQUID_INSPECT_FLOOR )
	DECLARE_ACTIVITY( ACT_SQUID_SPIT_OUT_CLAWS )

	//DECLARE_ANIMEVENT(AE_SQUID_SPIT_OUT_CLAWS)

	DECLARE_INTERACTION( g_interactionBullsquidThrow )

	//=========================================================
	// > SCHED_SQUID_HURTHOP
	//=========================================================
	/*DEFINE_SCHEDULE
	(
		SCHED_SQUID_HURTHOP,

		"	Tasks"
		"		TASK_STOP_MOVING			0"
		"		TASK_SOUND_WAKE				0"
		"		TASK_SQUID_HOPTURN			0"
		"		TASK_FACE_ENEMY				0"
		"	"
		"	Interrupts"
	);*/
	
	//=========================================================
	// > SCHED_SQUID_SEECRAB
	//=========================================================
	/*DEFINE_SCHEDULE
	(
		SCHED_SQUID_SEECRAB,

		"	Tasks"
		"		TASK_STOP_MOVING			0"
		"		TASK_SOUND_WAKE				0"
		"		TASK_PLAY_SEQUENCE			ACTIVITY:ACT_SQUID_EXCITED"
		"		TASK_FACE_ENEMY				0"
		"	"
		"	Interrupts"
		"		COND_LIGHT_DAMAGE"
		"		COND_HEAVY_DAMAGE"
	);*/
	
	//=========================================================
	// > SCHED_SQUID_EAT
	//=========================================================
	DEFINE_SCHEDULE
	(
		SCHED_SQUID_EAT,

		"	Tasks"
		"		TASK_STOP_MOVING					0"
		"		TASK_SQUID_EAT						10"
		"		TASK_STORE_LASTPOSITION				0"
		"		TASK_GET_PATH_TO_BESTSCENT			0"
		"		TASK_WALK_PATH						0"
		"		TASK_WAIT_FOR_MOVEMENT				0"
		"		TASK_PLAY_SEQUENCE					ACTIVITY:ACT_SQUID_EAT"
		"		TASK_PLAY_SEQUENCE					ACTIVITY:ACT_SQUID_EAT"
		"		TASK_PLAY_SEQUENCE					ACTIVITY:ACT_SQUID_EAT"
		"		TASK_SQUID_EAT						50"
		"		TASK_GET_PATH_TO_LASTPOSITION		0"
		"		TASK_WALK_PATH						0"
		"		TASK_WAIT_FOR_MOVEMENT				0"
		"		TASK_CLEAR_LASTPOSITION				0"
		"	"
		"	Interrupts"
		"		COND_LIGHT_DAMAGE"
		"		COND_HEAVY_DAMAGE"
		"		COND_NEW_ENEMY"
		"		COND_SMELL"
	);
	
	//=========================================================
	// > SCHED_SQUID_SNIFF_AND_EAT
	//=========================================================
	DEFINE_SCHEDULE
	(
		SCHED_SQUID_SNIFF_AND_EAT,

		"	Tasks"
		"		TASK_STOP_MOVING					0"
		"		TASK_SQUID_EAT						10"
		"		TASK_PLAY_SEQUENCE					ACTIVITY:ACT_SQUID_DETECT_SCENT"
		"		TASK_STORE_LASTPOSITION				0"
		"		TASK_GET_PATH_TO_BESTSCENT			0"
		"		TASK_WALK_PATH						0"
		"		TASK_WAIT_FOR_MOVEMENT				0"
		"		TASK_PLAY_SEQUENCE					ACTIVITY:ACT_SQUID_EAT"
		"		TASK_PLAY_SEQUENCE					ACTIVITY:ACT_SQUID_EAT"
		"		TASK_PLAY_SEQUENCE					ACTIVITY:ACT_SQUID_EAT"
		"		TASK_SQUID_EAT						50"
		"		TASK_GET_PATH_TO_LASTPOSITION		0"
		"		TASK_WALK_PATH						0"
		"		TASK_WAIT_FOR_MOVEMENT				0"
		"		TASK_CLEAR_LASTPOSITION				0"
		"	"
		"	Interrupts"
		"		COND_LIGHT_DAMAGE"
		"		COND_HEAVY_DAMAGE"
		"		COND_NEW_ENEMY"
		"		COND_SMELL"
	);
	
	//=========================================================
	// > SCHED_SQUID_WALLOW
	//=========================================================
	DEFINE_SCHEDULE
	(
		SCHED_SQUID_WALLOW,

		"	Tasks"
		"		TASK_STOP_MOVING				0"
		"		TASK_SQUID_EAT					10"
		"		TASK_STORE_LASTPOSITION			0"
		"		TASK_GET_PATH_TO_BESTSCENT		0"
		"		TASK_WALK_PATH					0"
		"		TASK_WAIT_FOR_MOVEMENT			0"
		"		TASK_PLAY_SEQUENCE				ACTIVITY:ACT_SQUID_INSPECT_FLOOR"
		"		TASK_SQUID_EAT					50"
		"		TASK_GET_PATH_TO_LASTPOSITION	0"
		"		TASK_WALK_PATH					0"
		"		TASK_WAIT_FOR_MOVEMENT			0"
		"		TASK_CLEAR_LASTPOSITION			0"
		"	"
		"	Interrupts"
		//"		COND_LIGHT_DAMAGE"
		//"		COND_HEAVY_DAMAGE"
		"		COND_NEW_ENEMY"
	);

	//=========================================================
	// > SCHED_SQUID_RANGE_ATTACK1
	//=========================================================
	/*DEFINE_SCHEDULE
	(
		SCHED_SQUID_SPIT_OUT_CLAWS,

		"	Tasks"
		"		TASK_STOP_MOVING		0"
		"		TASK_WAIT				1"
		"		TASK_PLAY_SEQUENCE		ACTIVITY:ACT_SQUID_SPIT_OUT_CLAWS"
		"		TASK_SUGGEST_STATE		STATE:ALERT"
		"	"
		"	Interrupts"
	);*/
	
AI_END_CUSTOM_NPC()