//========= Copyright Valve Corporation, All rights reserved. ============//
//
// Purpose: A slow-moving, once-human headcrab victim with only melee attacks.
//
//=============================================================================//

#include "cbase.h"

#include "doors.h"

#include "simtimer.h"
#include "npc_BaseZombie.h"
#include "ai_hull.h"
#include "ai_navigator.h"
#include "ai_memory.h"
#include "gib.h"
#include "soundenvelope.h"
#include "engine/IEngineSound.h"
#include "ammodef.h"


#include "datacache/imdlcache.h"
#include "particle_parse.h"
#include "te_particlesystem.h"
#include "SpriteTrail.h"
#include "IEffects.h"
#include "npc_bullseye.h"
#include "physobj.h"
#include "ai_memory.h"
#include "collisionutils.h"
#include "physics_prop_ragdoll.h"
#include "shot_manipulator.h"
#include "te_effect_dispatch.h"

// memdbgon must be the last include file in a .cpp file!!!
#include "tier0/memdbgon.h"

static const char *TASER_DART_MODEL = "models/weapons/hunter_flechette.mdl";

// Flechette volley attack
ConVar taser_dart_max_range("taser_dart_max_range", "1200");
ConVar taser_dart_min_range("taser_dart_min_range", "100");
ConVar taser_dart_volley_size("taser_dart_volley_size", "8");
ConVar taser_dart_speed("taser_dart_speed", "2000");
ConVar sk_taser_dmg_dart("sk_taser_dmg_dart", "4.0");
ConVar sk_taser_dart_explode_dmg("sk_taser_dart_explode_dmg", "12.0");
ConVar sk_taser_dart_explode_radius("sk_taser_dart_explode_radius", "128.0");
ConVar taser_dart_explode_delay("taser_dart_explode_delay", "2.5");
ConVar taser_dart_delay("taser_dart_delay", "0.1");
ConVar taser_first_dart_delay("taser_first_dart_delay", "0.5");
ConVar taser_dart_max_concurrent_volleys("taser_dart_max_concurrent_volleys", "2");
ConVar taser_dart_volley_start_min_delay("taser_dart_volley_start_min_delay", ".25");
ConVar taser_dart_volley_start_max_delay("taser_dart_volley_start_max_delay", ".95");
ConVar taser_dart_volley_end_min_delay("taser_dart_volley_end_min_delay", "1");
ConVar taser_dart_volley_end_max_delay("taser_dart_volley_end_max_delay", "2");
ConVar taser_dart_test("taser_dart_test", "0");
ConVar taser_clamp_shots("taser_clamp_shots", "1");
ConVar taser_cheap_explosions("taser_cheap_explosions", "1");

ConVar zombie_police_show_weapon_los_z("zombie_police_show_weapon_los_z", "0");
ConVar zombie_police_show_weapon_los_condition("zombie_police_show_weapon_los_condition", "0");

ConVar zombie_police_plant_adjust_z("zombie_police_plant_adjust_z", "12");

#define ZOMBIE_POLICE_FOV_DOT					0.0
#define ZOMBIE_POLICE_FACE_ENEMY_DIST			512.0f
#define TASER_DART_WARN_TIME		1.0f
#define ZOMBIE_POLICE_FACING_DOT				0.8		// The angle within which we start shooting
#define ZOMBIE_POLICE_SHOOT_MAX_YAW_DEG		60.0f	// Once shooting, clamp to +/- these degrees of yaw deflection as our target moves
#define ZOMBIE_POLICE_SHOOT_MAX_YAW_COS		0.5f	// The cosine of the above angle

Activity ACT_ZOMBIE_POLICE_TANTRUM;
Activity ACT_ZOMBIE_POLICE_WALK_ANGRY;
Activity ACT_ZOMBIE_POLICE_RANGE_ATTACK2_UNPLANTED;
Activity ACT_ZOMBIE_POLICE_IDLE_PLANTED;

//-----------------------------------------------------------------------------
// The hunter can fire a volley of explosive flechettes.
//-----------------------------------------------------------------------------
static const char *s_szTaserDartBubbles = "TaserDartBubbles";
static const char *s_szTaserDartSeekThink = "TaserDartSeekThink";
static const char *s_szTaserDartDangerSoundThink = "TaserDartDangerSoundThink";
static const char *s_szTaserDartSpriteTrail = "sprites/bluelaser1.vmt";
static int s_nTaserDartImpact = -2;
static int s_nFlechetteFuseAttach = -1;

#define DART_AIR_VELOCITY	2500

class CTaserDart : public CPhysicsProp, public IParentPropInteraction
{
	DECLARE_CLASS(CTaserDart, CPhysicsProp);

public:

	CTaserDart();
	~CTaserDart();

	Class_T Classify() { return CLASS_NONE; }

	bool WasThrownBack()
	{
		return m_bThrownBack;
	}

public:

	void Spawn();
	void Activate();
	void Precache();
	void Shoot(Vector &vecVelocity/*, bool bBright*/);
	void SetSeekTarget(CBaseEntity *pTargetEntity);
	void Explode();

	bool CreateVPhysics();

	unsigned int PhysicsSolidMaskForEntity() const;
	static CTaserDart *DartCreate(const Vector &vecOrigin, const QAngle &angAngles, CBaseEntity *pentOwner = NULL);

	// IParentPropInteraction
	void OnParentCollisionInteraction(parentCollisionInteraction_t eType, int index, gamevcollisionevent_t *pEvent);
	void OnParentPhysGunDrop(CBasePlayer *pPhysGunUser, PhysGunDrop_t Reason);

protected:

	void SetupGlobalModelData();

	void StickTo(CBaseEntity *pOther, trace_t &tr);

	void BubbleThink();
	void DangerSoundThink();
	void ExplodeThink();
	void DopplerThink();
	void SeekThink();

	bool CreateSprites(bool bBright);

	void DartTouch(CBaseEntity *pOther);

	Vector m_vecShootPosition;
	EHANDLE m_hSeekTarget;
	bool m_bThrownBack;

	DECLARE_DATADESC();
	//DECLARE_SERVERCLASS();
};

LINK_ENTITY_TO_CLASS(taser_dart, CTaserDart);

BEGIN_DATADESC(CTaserDart)

DEFINE_THINKFUNC(BubbleThink),
DEFINE_THINKFUNC(DangerSoundThink),
DEFINE_THINKFUNC(ExplodeThink),
DEFINE_THINKFUNC(DopplerThink),
DEFINE_THINKFUNC(SeekThink),

DEFINE_ENTITYFUNC(DartTouch),

DEFINE_FIELD(m_vecShootPosition, FIELD_POSITION_VECTOR),
DEFINE_FIELD(m_hSeekTarget, FIELD_EHANDLE),
DEFINE_FIELD(m_bThrownBack, FIELD_BOOLEAN),

END_DATADESC()

//IMPLEMENT_SERVERCLASS_ST( CTaserDart, DT_TaserDart )
//END_SEND_TABLE()


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
CTaserDart *CTaserDart::DartCreate(const Vector &vecOrigin, const QAngle &angAngles, CBaseEntity *pentOwner)
{
	// Create a new entity with CTaserDart private data
	CTaserDart *pDart = (CTaserDart *)CreateEntityByName("taser_dart");
	UTIL_SetOrigin(pDart, vecOrigin);
	pDart->SetAbsAngles(angAngles);
	pDart->Spawn();
	pDart->Activate();
	pDart->SetOwnerEntity(pentOwner);

	return pDart;
}


//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
void CC_Taser_Shoot_Dart(const CCommand& args)
{
	MDLCACHE_CRITICAL_SECTION();

	bool allowPrecache = CBaseEntity::IsPrecacheAllowed();
	CBaseEntity::SetAllowPrecache(true);

	CBasePlayer *pPlayer = UTIL_GetCommandClient();

	QAngle angEye = pPlayer->EyeAngles();
	CTaserDart *entity = CTaserDart::DartCreate(pPlayer->EyePosition(), angEye, pPlayer);
	if (entity)
	{
		entity->Precache();
		DispatchSpawn(entity);

		// Shoot the flechette.		
		Vector forward;
		pPlayer->EyeVectors(&forward);
		forward *= 2000.0f;
		entity->Shoot(forward/*, false*/);
	}

	CBaseEntity::SetAllowPrecache(allowPrecache);
}

static ConCommand ent_create("taser_shoot_dart", CC_Taser_Shoot_Dart, "Fires a taser dart where the player is looking.", FCVAR_GAMEDLL | FCVAR_CHEAT);


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
CTaserDart::CTaserDart()
{
	UseClientSideAnimation();
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
CTaserDart::~CTaserDart()
{
}


//-----------------------------------------------------------------------------
// If set, the flechette will seek unerringly toward the target as it flies.
//-----------------------------------------------------------------------------
void CTaserDart::SetSeekTarget(CBaseEntity *pTargetEntity)
{
	if (pTargetEntity)
	{
		m_hSeekTarget = pTargetEntity;
		SetContextThink(&CTaserDart::SeekThink, gpGlobals->curtime, s_szTaserDartSeekThink);
	}
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
bool CTaserDart::CreateVPhysics()
{
	// Create the object in the physics system
	VPhysicsInitNormal(SOLID_BBOX, FSOLID_NOT_STANDABLE, false);

	return true;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
unsigned int CTaserDart::PhysicsSolidMaskForEntity() const
{
	return (BaseClass::PhysicsSolidMaskForEntity() | CONTENTS_HITBOX) & ~CONTENTS_GRATE;
}


//-----------------------------------------------------------------------------
// Called from CPropPhysics code when we're attached to a physics object.
//-----------------------------------------------------------------------------
void CTaserDart::OnParentCollisionInteraction(parentCollisionInteraction_t eType, int index, gamevcollisionevent_t *pEvent)
{
	if (eType == COLLISIONINTER_PARENT_FIRST_IMPACT)
	{
		m_bThrownBack = true;
		Explode();
	}
}

void CTaserDart::OnParentPhysGunDrop(CBasePlayer *pPhysGunUser, PhysGunDrop_t Reason)
{
	m_bThrownBack = true;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
bool CTaserDart::CreateSprites(bool bBright)
{
	/*if (bBright)
	{
		DispatchParticleEffect("taser_dart_trail_striderbuster", PATTACH_ABSORIGIN_FOLLOW, this);
	}
	else
	{*/
		DispatchParticleEffect("taser_dart_trail", PATTACH_ABSORIGIN_FOLLOW, this);
	//}

	return true;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::Spawn()
{
	Precache();

	SetModel(TASER_DART_MODEL);
	SetMoveType(MOVETYPE_FLYGRAVITY, MOVECOLLIDE_FLY_CUSTOM);
	UTIL_SetSize(this, -Vector(1, 1, 1), Vector(1, 1, 1));
	SetSolid(SOLID_BBOX);
	SetGravity(0.05f);
	SetCollisionGroup(COLLISION_GROUP_PROJECTILE);

	// Make sure we're updated if we're underwater
	UpdateWaterState();

	SetTouch(&CTaserDart::DartTouch);

	// Make us glow until we've hit the wall
	m_nSkin = 1;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::Activate()
{
	BaseClass::Activate();
	SetupGlobalModelData();
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::SetupGlobalModelData()
{
	if (s_nTaserDartImpact == -2)
	{
		s_nTaserDartImpact = LookupSequence("impact");
		s_nFlechetteFuseAttach = LookupAttachment("attach_fuse");
	}
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::Precache()
{
	PrecacheModel(TASER_DART_MODEL);
	PrecacheModel("sprites/light_glow02_noz.vmt");

	PrecacheScriptSound("NPC_Hunter.FlechetteNearmiss");
	PrecacheScriptSound("NPC_Hunter.FlechetteHitBody");
	PrecacheScriptSound("NPC_Hunter.FlechetteHitWorld");
	PrecacheScriptSound("NPC_Hunter.FlechettePreExplode");
	PrecacheScriptSound("NPC_Hunter.FlechetteExplode");

	//PrecacheParticleSystem("taser_dart_trail_striderbuster");
	PrecacheParticleSystem("taser_dart_trail");
	PrecacheParticleSystem("hunter_projectile_explosion_1");
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::StickTo(CBaseEntity *pOther, trace_t &tr)
{
	EmitSound("NPC_Hunter.FlechetteHitWorld");

	SetMoveType(MOVETYPE_NONE);

	if (!pOther->IsWorld())
	{
		SetParent(pOther);
		SetSolid(SOLID_NONE);
		SetSolidFlags(FSOLID_NOT_SOLID);
	}

	// Do an impact effect.
	//Vector vecDir = GetAbsVelocity();
	//float speed = VectorNormalize( vecDir );

	//Vector vForward;
	//AngleVectors( GetAbsAngles(), &vForward );
	//VectorNormalize ( vForward );

	//CEffectData	data;
	//data.m_vOrigin = tr.endpos;
	//data.m_vNormal = vForward;
	//data.m_nEntIndex = 0;
	//DispatchEffect( "BoltImpact", data );

	Vector vecVelocity = GetAbsVelocity();
	//bool bAttachedToBuster = StriderBuster_OnFlechetteAttach(pOther, vecVelocity);

	SetTouch(NULL);

	// We're no longer flying. Stop checking for water volumes.
	SetContextThink(NULL, 0, s_szTaserDartBubbles);

	// Stop seeking.
	m_hSeekTarget = NULL;
	SetContextThink(NULL, 0, s_szTaserDartSeekThink);

	// Get ready to explode.
	/*if (!bAttachedToBuster)
	{
		SetThink(&CTaserDart::DangerSoundThink);
		SetNextThink(gpGlobals->curtime + (taser_dart_explode_delay.GetFloat() - TASER_DART_WARN_TIME));
	}
	else
	{*/
		DangerSoundThink();
	//}

	// Play our impact animation.
	ResetSequence(s_nTaserDartImpact);

	static int s_nImpactCount = 0;
	s_nImpactCount++;
	if (s_nImpactCount & 0x01)
	{
		UTIL_ImpactTrace(&tr, DMG_BULLET);

		// Shoot some sparks
		if (UTIL_PointContents(GetAbsOrigin()) != CONTENTS_WATER)
		{
			g_pEffects->Sparks(GetAbsOrigin());
		}
	}
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::DartTouch(CBaseEntity *pOther)
{
	if (pOther->IsSolidFlagSet(FSOLID_VOLUME_CONTENTS | FSOLID_TRIGGER))
	{
		// Some NPCs are triggers that can take damage (like antlion grubs). We should hit them.
		if ((pOther->m_takedamage == DAMAGE_NO) || (pOther->m_takedamage == DAMAGE_EVENTS_ONLY))
			return;
	}

	if (FClassnameIs(pOther, "taser_dart"))
		return;

	trace_t	tr;
	tr = BaseClass::GetTouchTrace();

	if (pOther->m_takedamage != DAMAGE_NO)
	{
		Vector	vecNormalizedVel = GetAbsVelocity();

		ClearMultiDamage();
		VectorNormalize(vecNormalizedVel);

		float flDamage = sk_taser_dmg_dart.GetFloat();
		CBreakable *pBreak = dynamic_cast <CBreakable *>(pOther);
		if (pBreak && (pBreak->GetMaterialType() == matGlass))
		{
			flDamage = MAX(pOther->GetHealth(), flDamage);
		}

		CTakeDamageInfo	dmgInfo(this, GetOwnerEntity(), flDamage, DMG_DISSOLVE | DMG_NEVERGIB);
		CalculateMeleeDamageForce(&dmgInfo, vecNormalizedVel, tr.endpos, 0.7f);
		dmgInfo.SetDamagePosition(tr.endpos);
		pOther->DispatchTraceAttack(dmgInfo, vecNormalizedVel, &tr);

		ApplyMultiDamage();

		// Keep going through breakable glass.
		if (pOther->GetCollisionGroup() == COLLISION_GROUP_BREAKABLE_GLASS)
			return;

		SetAbsVelocity(Vector(0, 0, 0));

		// play body "thwack" sound
		EmitSound("NPC_Hunter.FlechetteHitBody");

		StopParticleEffects(this);

		Vector vForward;
		AngleVectors(GetAbsAngles(), &vForward);
		VectorNormalize(vForward);

		trace_t	tr2;
		UTIL_TraceLine(GetAbsOrigin(), GetAbsOrigin() + vForward * 128, MASK_BLOCKLOS, pOther, COLLISION_GROUP_NONE, &tr2);

		if (tr2.fraction != 1.0f)
		{
			//NDebugOverlay::Box( tr2.endpos, Vector( -16, -16, -16 ), Vector( 16, 16, 16 ), 0, 255, 0, 0, 10 );
			//NDebugOverlay::Box( GetAbsOrigin(), Vector( -16, -16, -16 ), Vector( 16, 16, 16 ), 0, 0, 255, 0, 10 );

			if (tr2.m_pEnt == NULL || (tr2.m_pEnt && tr2.m_pEnt->GetMoveType() == MOVETYPE_NONE))
			{
				CEffectData	data;

				data.m_vOrigin = tr2.endpos;
				data.m_vNormal = vForward;
				data.m_nEntIndex = tr2.fraction != 1.0f;

				//DispatchEffect( "BoltImpact", data );
			}
		}

		if (((pOther->GetMoveType() == MOVETYPE_VPHYSICS) || (pOther->GetMoveType() == MOVETYPE_PUSH)) && ((pOther->GetHealth() > 0) || (pOther->m_takedamage == DAMAGE_EVENTS_ONLY)))
		{
			CPhysicsProp *pProp = dynamic_cast<CPhysicsProp *>(pOther);
			if (pProp)
			{
				pProp->SetInteraction(PROPINTER_PHYSGUN_NOTIFY_CHILDREN);
			}

			// We hit a physics object that survived the impact. Stick to it.
			StickTo(pOther, tr);
		}
		else
		{
			SetTouch(NULL);
			SetThink(NULL);
			SetContextThink(NULL, 0, s_szTaserDartBubbles);

			UTIL_Remove(this);
		}
	}
	else
	{
		// See if we struck the world
		if (pOther->GetMoveType() == MOVETYPE_NONE && !(tr.surface.flags & SURF_SKY))
		{
			// We hit a physics object that survived the impact. Stick to it.
			StickTo(pOther, tr);
		}
		else if (pOther->GetMoveType() == MOVETYPE_PUSH && FClassnameIs(pOther, "func_breakable"))
		{
			// We hit a func_breakable, stick to it.
			// The MOVETYPE_PUSH is a micro-optimization to cut down on the classname checks.
			StickTo(pOther, tr);
		}
		else
		{
			// Put a mark unless we've hit the sky
			if ((tr.surface.flags & SURF_SKY) == false)
			{
				UTIL_ImpactTrace(&tr, DMG_BULLET);
			}

			UTIL_Remove(this);
		}
	}
}


//-----------------------------------------------------------------------------
// Fixup flechette position when seeking towards a striderbuster.
//-----------------------------------------------------------------------------
void CTaserDart::SeekThink()
{
	if (m_hSeekTarget)
	{
		Vector vecBodyTarget = m_hSeekTarget->BodyTarget(GetAbsOrigin());

		Vector vecClosest;
		CalcClosestPointOnLineSegment(GetAbsOrigin(), m_vecShootPosition, vecBodyTarget, vecClosest, NULL);

		Vector vecDelta = vecBodyTarget - m_vecShootPosition;
		VectorNormalize(vecDelta);

		QAngle angShoot;
		VectorAngles(vecDelta, angShoot);

		float flSpeed = taser_dart_speed.GetFloat();
		if (!flSpeed)
		{
			flSpeed = 2500.0f;
		}

		Vector vecVelocity = vecDelta * flSpeed;
		Teleport(&vecClosest, &angShoot, &vecVelocity);

		SetNextThink(gpGlobals->curtime, s_szTaserDartSeekThink);
	}
}


//-----------------------------------------------------------------------------
// Play a near miss sound as we travel past the player.
//-----------------------------------------------------------------------------
void CTaserDart::DopplerThink()
{
	CBasePlayer *pPlayer = AI_GetSinglePlayer();
	if (!pPlayer)
		return;

	Vector vecVelocity = GetAbsVelocity();
	VectorNormalize(vecVelocity);

	float flMyDot = DotProduct(vecVelocity, GetAbsOrigin());
	float flPlayerDot = DotProduct(vecVelocity, pPlayer->GetAbsOrigin());

	if (flPlayerDot <= flMyDot)
	{
		EmitSound("NPC_Hunter.FlechetteNearMiss");

		// We've played the near miss sound and we're not seeking. Stop thinking.
		SetThink(NULL);
	}
	else
	{
		SetNextThink(gpGlobals->curtime);
	}
}


//-----------------------------------------------------------------------------
// Think every 0.1 seconds to make bubbles if we're flying through water.
//-----------------------------------------------------------------------------
void CTaserDart::BubbleThink()
{
	SetNextThink(gpGlobals->curtime + 0.1f, s_szTaserDartBubbles);

	if (GetWaterLevel() == 0)
		return;

	UTIL_BubbleTrail(GetAbsOrigin() - GetAbsVelocity() * 0.1f, GetAbsOrigin(), 5);
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::Shoot(Vector &vecVelocity/*, bool bBrightFX*/)
{
	//CreateSprites(bBrightFX);

	m_vecShootPosition = GetAbsOrigin();

	SetAbsVelocity(vecVelocity);

	SetThink(&CTaserDart::DopplerThink);
	SetNextThink(gpGlobals->curtime);

	SetContextThink(&CTaserDart::BubbleThink, gpGlobals->curtime + 0.1, s_szTaserDartBubbles);
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::DangerSoundThink()
{
	EmitSound("NPC_Hunter.FlechettePreExplode");

	CSoundEnt::InsertSound(SOUND_DANGER | SOUND_CONTEXT_EXCLUDE_COMBINE, GetAbsOrigin(), 150.0f, 0.5, this);
	SetThink(&CTaserDart::ExplodeThink);
	SetNextThink(gpGlobals->curtime + TASER_DART_WARN_TIME);
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::ExplodeThink()
{
	Explode();
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CTaserDart::Explode()
{
	SetSolid(SOLID_NONE);

	// Don't catch self in own explosion!
	m_takedamage = DAMAGE_NO;

	EmitSound("NPC_Hunter.FlechetteExplode");

	// Move the explosion effect to the tip to reduce intersection with the world.
	Vector vecFuse;
	GetAttachment(s_nFlechetteFuseAttach, vecFuse);
	DispatchParticleEffect("hunter_projectile_explosion_1", vecFuse, GetAbsAngles(), NULL);

	int nDamageType = DMG_DISSOLVE;

	// Perf optimization - only every other explosion makes a physics force. This is
	// hardly noticeable since flechettes usually explode in clumps.
	static int s_nExplosionCount = 0;
	s_nExplosionCount++;
	if ((s_nExplosionCount & 0x01) && taser_cheap_explosions.GetBool())
	{
		nDamageType |= DMG_PREVENT_PHYSICS_FORCE;
	}

	RadiusDamage(CTakeDamageInfo(this, GetOwnerEntity(), sk_taser_dart_explode_dmg.GetFloat(), nDamageType), GetAbsOrigin(), sk_taser_dart_explode_radius.GetFloat(), CLASS_NONE, NULL);

	AddEffects(EF_NODRAW);

	SetThink(&CBaseEntity::SUB_Remove);
	SetNextThink(gpGlobals->curtime + 0.1f);
}

// break

// ACT_FLINCH_PHYSICS

ConVar	sk_zombie_police_health("sk_zombie_police_health", "0");

envelopePoint_t envZombiePoliceMoanVolumeFast[] =
{
	{	7.0f, 7.0f,
		0.1f, 0.1f,
	},
	{	0.0f, 0.0f,
		0.2f, 0.3f,
	},
};

envelopePoint_t envZombiePoliceMoanVolume[] =
{
	{	1.0f, 1.0f,
		0.1f, 0.1f,
	},
	{	1.0f, 1.0f,
		0.2f, 0.2f,
	},
	{	0.0f, 0.0f,
		0.3f, 0.4f,
	},
};

envelopePoint_t envZombiePoliceMoanVolumeLong[] =
{
	{	1.0f, 1.0f,
		0.3f, 0.5f,
	},
	{	1.0f, 1.0f,
		0.6f, 1.0f,
	},
	{	0.0f, 0.0f,
		0.3f, 0.4f,
	},
};

envelopePoint_t envZombiePoliceMoanIgnited[] =
{
	{	1.0f, 1.0f,
		0.5f, 1.0f,
	},
	{	1.0f, 1.0f,
		30.0f, 30.0f,
	},
	{	0.0f, 0.0f,
		0.5f, 1.0f,
	},
};


//=============================================================================
//=============================================================================

class CZombiePolice : public CAI_BlendingHost<CNPC_BaseZombie>
{
	DECLARE_DATADESC();
	DECLARE_CLASS( CZombiePolice, CAI_BlendingHost<CNPC_BaseZombie> );

public:
	CZombiePolice()
	 : m_DurationDoorBash( 2, 6),
	   m_NextTimeToStartDoorBash( 3.0 )
	{
	}

	void NPCThink();
	void Spawn( void );
	void Precache( void );

	void SetZombieModel( void );
	void MoanSound( envelopePoint_t *pEnvelope, int iEnvelopeSize );
	bool ShouldBecomeTorso( const CTakeDamageInfo &info, float flDamageThreshold );
	bool CanBecomeLiveTorso() { return !m_fIsHeadless; }

	void GatherConditions( void );

	int SelectFailSchedule( int failedSchedule, int failedTask, AI_TaskFailureCode_t taskFailCode );
	int TranslateSchedule( int scheduleType );

#ifndef HL2_EPISODIC
	void CheckFlinches() {} // Zombie has custom flinch code
#endif // HL2_EPISODIC

	Activity NPC_TranslateActivity( Activity newActivity );

	void OnStateChange( NPC_STATE OldState, NPC_STATE NewState );

	void StartTask( const Task_t *pTask );
	void RunTask( const Task_t *pTask );

	virtual const char *GetLegsModel( void );
	virtual const char *GetTorsoModel( void );
	virtual const char *GetHeadcrabClassname( void );
	virtual const char *GetHeadcrabModel( void );

	virtual bool OnObstructingDoor( AILocalMoveGoal_t *pMoveGoal, 
								 CBaseDoor *pDoor,
								 float distClear, 
								 AIMoveResult_t *pResult );

	Activity SelectDoorBash();

	void Ignite( float flFlameLifetime, bool bNPCOnly = true, float flSize = 0.0f, bool bCalledByLevelDesigner = false );
	void Extinguish();
	int OnTakeDamage_Alive( const CTakeDamageInfo &inputInfo );
	bool IsHeavyDamage( const CTakeDamageInfo &info );
	bool IsSquashed( const CTakeDamageInfo &info );
	void BuildScheduleTestBits( void );

	void PrescheduleThink( void );
	int SelectSchedule ( void );

	int	SelectCombatSchedule();
	void			SetupGlobalModelData();

	void PainSound( const CTakeDamageInfo &info );
	void DeathSound( const CTakeDamageInfo &info );
	void AlertSound( void );
	void IdleSound( void );
	void AttackSound( void );
	void AttackHitSound( void );
	void AttackMissSound( void );
	void FootstepSound( bool fRightFoot );
	void FootscuffSound( bool fRightFoot );

	const char *GetMoanSound( int nSound );


	int				RangeAttack1Conditions(float flDot, float flDist);
	int				RangeAttack2Conditions(float flDot, float flDist);

	bool			WeaponLOSCondition(const Vector &ownerPos, const Vector &targetPos, bool bSetConditions);
	bool			TestShootPosition(const Vector &vecShootPos, const Vector &targetPos);

	Vector			Weapon_ShootPosition();

	CBaseEntity *	MeleeAttack(float flDist, int iDamage, QAngle &qaViewPunch, Vector &vecVelocityPunch, int BloodOrigin);

	void			MakeTracer(const Vector &vecTracerSrc, const trace_t &tr, int iTracerType);
	void			DoMuzzleFlash(int nAttachment);
	bool			CanShootThrough(const trace_t &tr, const Vector &vecTarget);
	int				CountRangedAttackers();
	void 			DelayRangedAttackers(float minDelay, float maxDelay, bool bForced = false);
	int				DrawDebugTextOverlays();

	void			InputEnableUnplantedShooting(inputdata_t &inputdata);
	void			InputDisableUnplantedShooting(inputdata_t &inputdata);
	
public:
	DEFINE_CUSTOM_AI;

protected:
	static const char *pMoanSounds[];


private:
	CHandle< CBaseDoor > m_hBlockingDoor;
	float				 m_flDoorBashYaw;
	
	CRandSimTimer 		 m_DurationDoorBash;
	CSimTimer 	  		 m_NextTimeToStartDoorBash;

	Vector				 m_vPositionCharged;

	Vector m_vecEnemyLastSeen;

	void BeginVolley(int nNum, float flStartTime);
	bool ShootDart(CBaseEntity *pTargetEntity, bool bSingleShot);
	void GetShootDir(Vector &vecDir, const Vector &vecSrc, CBaseEntity *pTargetEntity, /*bool bStriderbuster,*/ int nShotNum, bool bSingleShot);
	bool ClampShootDir(Vector &vecDir);

	int m_nDartsQueued;
	int m_nClampedShots;				// The number of consecutive shots fired at an out-of-max yaw target.

	float m_flNextRangeAttack2Time;		// Time when we can fire another volley of flechettes.
	float m_flNextDartTime;		// Time to fire the next flechette in this volley.

	bool m_bPlanted;
	bool m_bLastCanPlantHere;
	bool m_bMissLeft;
	bool m_bEnableUnplantedShooting;
	float m_flShootAllowInterruptTime;
	static int gm_nTaserGunAttachment;
	bool CanPlantHere(const Vector &vecPos);

	static int gm_nUnplantedNode;
	static int gm_nPlantedNode;
};

LINK_ENTITY_TO_CLASS( npc_zombie_police, CZombiePolice );
LINK_ENTITY_TO_CLASS( npc_zombie_police_torso, CZombiePolice );

//---------------------------------------------------------
//---------------------------------------------------------
const char *CZombiePolice::pMoanSounds[] =
{
	 "NPC_BaseZombie.Moan1",
	 "NPC_BaseZombie.Moan2",
	 "NPC_BaseZombie.Moan3",
	 "NPC_BaseZombie.Moan4",
};

//=========================================================
// Conditions
//=========================================================
enum
{
	COND_BLOCKED_BY_DOOR = LAST_BASE_ZOMBIE_CONDITION,
	COND_DOOR_OPENED,
	COND_ZOMBIE_CHARGE_TARGET_MOVED,

	COND_ZOMBIE_POLICE_CANT_PLANT,
};

//=========================================================
// Schedules
//=========================================================
enum
{
	SCHED_ZOMBIE_BASH_DOOR = LAST_BASE_ZOMBIE_SCHEDULE,
	SCHED_ZOMBIE_WANDER_ANGRILY,
	SCHED_ZOMBIE_CHARGE_ENEMY,
	SCHED_ZOMBIE_FAIL,

	SCHED_ZOMBIE_POLICE_RANGE_ATTACK2,
};

//=========================================================
// Tasks
//=========================================================
enum
{
	TASK_ZOMBIE_EXPRESS_ANGER = LAST_BASE_ZOMBIE_TASK,
	TASK_ZOMBIE_YAW_TO_DOOR,
	TASK_ZOMBIE_ATTACK_DOOR,
	TASK_ZOMBIE_CHARGE_ENEMY,

	TASK_ZOMBIE_POLICE_PRE_RANGE_ATTACK2,
	TASK_ZOMBIE_POLICE_SHOOT_COMMIT,
};

//-----------------------------------------------------------------------------

//int ACT_ZOMBIE_POLICE_TANTRUM;
int ACT_ZOMBIE_POLICE_WALLPOUND;

int CZombiePolice::gm_nTaserGunAttachment = -1;
int CZombiePolice::gm_nUnplantedNode = 0;
int CZombiePolice::gm_nPlantedNode = 0;

BEGIN_DATADESC( CZombiePolice )

	DEFINE_FIELD( m_hBlockingDoor, FIELD_EHANDLE ),
	DEFINE_FIELD( m_flDoorBashYaw, FIELD_FLOAT ),
	DEFINE_EMBEDDED( m_DurationDoorBash ),
	DEFINE_EMBEDDED( m_NextTimeToStartDoorBash ),
	DEFINE_FIELD( m_vPositionCharged, FIELD_POSITION_VECTOR ),

	DEFINE_FIELD(m_nDartsQueued, FIELD_INTEGER),
	DEFINE_FIELD(m_nClampedShots, FIELD_INTEGER),
	DEFINE_FIELD(m_flNextRangeAttack2Time, FIELD_TIME),
	DEFINE_FIELD(m_flNextDartTime, FIELD_TIME),

	DEFINE_INPUTFUNC(FIELD_VOID, "EnableUnplantedShooting", InputEnableUnplantedShooting),
	DEFINE_INPUTFUNC(FIELD_VOID, "DisableUnplantedShooting", InputDisableUnplantedShooting),

END_DATADESC()

//-----------------------------------------------------------------------------
// Purpose: 
//-----------------------------------------------------------------------------
void CZombiePolice::Precache( void )
{
	BaseClass::Precache();

	PrecacheModel( "models/zombie/zombie_civ_police.mdl" );
	PrecacheModel( "models/zombie/zombie_civ_police_torso.mdl" );
	PrecacheModel( "models/zombie/zombie_civ_police_legs.mdl" );

	//PrecacheScriptSound( "Zombie.FootstepRight" );
	//PrecacheScriptSound( "Zombie.FootstepLeft" );
	//PrecacheScriptSound( "Zombie.FootstepLeft" );
	PrecacheScriptSound( "Zombie.ScuffRight" );
	PrecacheScriptSound( "Zombie.ScuffLeft" );
	PrecacheScriptSound( "Zombie.AttackHit" );
	PrecacheScriptSound( "Zombie.AttackMiss" );
	PrecacheScriptSound( "Zombie.Pain" );
	PrecacheScriptSound( "Zombie.Die" );
	PrecacheScriptSound( "Zombie.Alert" );
	PrecacheScriptSound( "Zombie.Idle" );
	PrecacheScriptSound( "Zombie.Attack" );

	PrecacheScriptSound("NPC_MetroPolice.RunFootstepLeft");
	PrecacheScriptSound("NPC_MetroPolice.RunFootstepRight");


	PrecacheScriptSound( "NPC_BaseZombie.Moan1" );
	PrecacheScriptSound( "NPC_BaseZombie.Moan2" );
	PrecacheScriptSound( "NPC_BaseZombie.Moan3" );
	PrecacheScriptSound( "NPC_BaseZombie.Moan4" );

	UTIL_PrecacheOther("taser_dart");
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::Spawn( void )
{
	Precache();

	if( FClassnameIs( this, "npc_zombie_police" ) )
	{
		m_fIsTorso = false;
	}
	else
	{
		// This was placed as an npc_zombie_torso
		m_fIsTorso = true;
	}

	m_fIsHeadless = false;

	SetupGlobalModelData();

//#ifdef HL2_EPISODIC
	//SetBloodColor( BLOOD_COLOR_ZOMBIE );
//#else
	SetBloodColor( BLOOD_COLOR_GREEN );
//#endif // HL2_EPISODIC

	m_iHealth			= sk_zombie_police_health.GetFloat();
	//m_flFieldOfView		= 0.2;

	m_flFieldOfView = ZOMBIE_POLICE_FOV_DOT;

	m_flDistTooFar = taser_dart_max_range.GetFloat();

	//CapabilitiesClear();

	CapabilitiesAdd(bits_CAP_INNATE_RANGE_ATTACK1 | bits_CAP_INNATE_RANGE_ATTACK2 | bits_CAP_INNATE_MELEE_ATTACK1);

	//GetNavigator()->SetRememberStaleNodes( false );

	BaseClass::Spawn();

	m_flNextMoanSound = gpGlobals->curtime + random->RandomFloat( 1.0, 4.0 );
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::SetupGlobalModelData()
{
	gm_nTaserGunAttachment = LookupAttachment("maw");

	int nSequence = SelectWeightedSequence(ACT_ZOMBIE_POLICE_RANGE_ATTACK2_UNPLANTED);
	gm_nUnplantedNode = GetEntryNode(nSequence);

	nSequence = SelectWeightedSequence(ACT_RANGE_ATTACK2);
	gm_nPlantedNode = GetEntryNode(nSequence);

	CollisionProp()->SetSurroundingBoundsType(USE_HITBOXES);
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::NPCThink()
{
	BaseClass::NPCThink();

	// Update our planted/unplanted state.
	m_bPlanted = (GetEntryNode(GetSequence()) == gm_nPlantedNode);

	//UpdateAim();
	//UpdateEyes();
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::PrescheduleThink( void )
{
  	if( gpGlobals->curtime > m_flNextMoanSound )
  	{
  		if( CanPlayMoanSound() )
  		{
			// Classic guy idles instead of moans.
			IdleSound();

  			m_flNextMoanSound = gpGlobals->curtime + random->RandomFloat( 2.0, 5.0 );
  		}
  		else
 		{
  			m_flNextMoanSound = gpGlobals->curtime + random->RandomFloat( 1.0, 2.0 );
  		}
  	}

	BaseClass::PrescheduleThink();
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
int	CZombiePolice::DrawDebugTextOverlays()
{
	int text_offset = BaseClass::DrawDebugTextOverlays();

	if (m_debugOverlays & OVERLAY_TEXT_BIT)
	{
		EntityText(text_offset, CFmtStr("%s", m_bPlanted ? "Planted" : "Unplanted"), 0);
		text_offset++;

		//EntityText(text_offset, CFmtStr("Eye state: %d", m_eEyeState), 0);
		//text_offset++;

		/*if (IsUsingSiegeTargets())
		{
			EntityText(text_offset, CFmtStr("Next Siege Attempt:%f", m_flTimeNextSiegeTargetAttack - gpGlobals->curtime), 0);
			text_offset++;
		}*/
	}

	return text_offset;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
int CZombiePolice::SelectCombatSchedule()
{
	if (taser_dart_test.GetBool())
	{
		if (HasCondition(COND_CAN_RANGE_ATTACK2))
		{
			return SCHED_ZOMBIE_POLICE_RANGE_ATTACK2;
		}
		return SCHED_COMBAT_FACE;
	}

	return SCHED_ZOMBIE_WANDER_ANGRILY;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
int CZombiePolice::SelectSchedule ( void )
{
	if( HasCondition( COND_PHYSICS_DAMAGE ) && !m_ActBusyBehavior.IsActive() )
	{
		return SCHED_FLINCH_PHYSICS;
	}

	return BaseClass::SelectSchedule();
}

//-----------------------------------------------------------------------------
// Given a target to shoot at, decide where to aim.
//-----------------------------------------------------------------------------
void CZombiePolice::GetShootDir(Vector &vecDir, const Vector &vecSrc, CBaseEntity *pTargetEntity, /*bool bStriderBuster,*/ int nShotNum, bool bSingleShot)
{
	//RestartGesture( ACT_HUNTER_GESTURE_SHOOT );

	EmitSound("NPC_Hunter.FlechetteShoot");

	Vector vecBodyTarget;

	if (pTargetEntity->Classify() == CLASS_PLAYER_ALLY_VITAL)
	{
		// Shooting at Alyx, most likely (in EP2). The attack is designed to displace
		// her, not necessarily actually harm her. So shoot at the area around her feet.
		vecBodyTarget = pTargetEntity->GetAbsOrigin();
	}
	else
	{
		vecBodyTarget = pTargetEntity->BodyTarget(vecSrc);
	}

	Vector vecTarget = vecBodyTarget;

	Vector vecDelta = pTargetEntity->GetAbsOrigin() - GetAbsOrigin();
	float flDist = vecDelta.Length();

	//if (!bStriderBuster)
	//{
		// If we're not firing at a strider buster, miss in an entertaining way for the 
		// first three shots of each volley.
		if ((nShotNum < 3) && (flDist > 200))
		{
			Vector vecTargetForward;
			Vector vecTargetRight;
			pTargetEntity->GetVectors(&vecTargetForward, &vecTargetRight, NULL);

			Vector vecForward;
			GetVectors(&vecForward, NULL, NULL);

			float flDot = DotProduct(vecTargetForward, vecForward);

			if (flDot < -0.8f)
			{
				// Our target is facing us, shoot the ground between us.
				float flPerc = 0.7 + (0.1 * nShotNum);
				vecTarget = GetAbsOrigin() + (flPerc * (pTargetEntity->GetAbsOrigin() - GetAbsOrigin()));
			}
			else if (flDot > 0.8f)
			{
				// Our target is facing away from us, shoot to the left or right.
				Vector vecMissDir = vecTargetRight;
				if (m_bMissLeft)
				{
					vecMissDir *= -1.0f;
				}

				vecTarget = pTargetEntity->EyePosition() + (36.0f * (3 - nShotNum)) * vecMissDir;
			}
			else
			{
				// Our target is facing vaguely perpendicular to us, shoot across their view.
				vecTarget = pTargetEntity->EyePosition() + (36.0f * (3 - nShotNum)) * vecTargetForward;
			}
		}
		// If we can't see them, shoot where we last saw them.
		else if (!HasCondition(COND_SEE_ENEMY))
		{
			Vector vecDelta = vecTarget - pTargetEntity->GetAbsOrigin();
			vecTarget = m_vecEnemyLastSeen + vecDelta;
		}
	/*}
	else
	{
		// If we're firing at a striderbuster, lead it.
		float flSpeed = taser_dart_speed.GetFloat();
		if (!flSpeed)
		{
			flSpeed = 2500.0f;
		}

		flSpeed *= 1.5;

		float flDeltaTime = flDist / flSpeed;
		vecTarget = vecTarget + flDeltaTime * pTargetEntity->GetSmoothedVelocity();
	}*/

	vecDir = vecTarget - vecSrc;
	VectorNormalize(vecDir);
}


//-----------------------------------------------------------------------------
// Ensures that we don't exceed our pitch/yaw limits when shooting flechettes.
// Returns true if we had to clamp, false if not.
//-----------------------------------------------------------------------------
bool CZombiePolice::ClampShootDir(Vector &vecDir)
{
	Vector vecDir2D = vecDir;
	vecDir2D.z = 0;

	Vector vecForward;
	GetVectors(&vecForward, NULL, NULL);

	Vector vecForward2D = vecForward;
	vecForward2D.z = 0;

	float flDot = DotProduct(vecForward2D, vecDir2D);
	if (flDot >= ZOMBIE_POLICE_SHOOT_MAX_YAW_COS)
	{
		// No need to clamp.
		return false;
	}

	Vector vecAxis;
	CrossProduct(vecDir, vecForward, vecAxis);
	VectorNormalize(vecAxis);

	Quaternion q;
	AxisAngleQuaternion(vecAxis, -ZOMBIE_POLICE_SHOOT_MAX_YAW_DEG, q);

	matrix3x4_t rot;
	QuaternionMatrix(q, rot);
	VectorRotate(vecForward, rot, vecDir);
	VectorNormalize(vecDir);

	return true;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::BeginVolley(int nNum, float flStartTime)
{
	m_nDartsQueued = nNum;
	m_nClampedShots = 0;
	m_flNextDartTime = flStartTime;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
bool CZombiePolice::ShootDart(CBaseEntity *pTargetEntity, bool bSingleShot)
{
	if (!pTargetEntity)
	{
		Assert(false);
		return false;
	}

	int nShotNum = taser_dart_volley_size.GetInt() - m_nDartsQueued;

	//bool bStriderBuster = IsStriderBuster(pTargetEntity);

	// Choose the next muzzle to shoot from.
	Vector vecSrc;
	QAngle angMuzzle;

	/*if (m_bTopMuzzle)
	{
		GetAttachment(gm_nTopGunAttachment, vecSrc, angMuzzle);
		DoMuzzleFlash(gm_nTopGunAttachment);
	}
	else
	{*/
		GetAttachment(gm_nTaserGunAttachment, vecSrc, angMuzzle);
		//DoMuzzleFlash(gm_nTaserGunAttachment);
	//}

	//m_bTopMuzzle = !m_bTopMuzzle;

	Vector vecDir;
	GetShootDir(vecDir, vecSrc, pTargetEntity, /*bStriderBuster,*/ nShotNum, bSingleShot);

	bool bClamped = false;
	if (taser_clamp_shots.GetBool())
	{
		bClamped = ClampShootDir(vecDir);
	}

	CShotManipulator manipulator(vecDir);
	Vector vecShoot;

	/*if (IsUsingSiegeTargets() && nShotNum >= 2 && (nShotNum % 2) == 0)
	{
		// Near perfect accuracy for these three shots, so they are likely to fly right into the windows.
		// NOTE! In siege behavior in the map that this behavior was designed for (ep2_outland_10), the
		// Hunters will only ever shoot at siege targets in siege mode. If you allow Hunters in siege mode
		// to attack players or other NPCs, this accuracy bonus will apply unless we apply a bit more logic to it.
		vecShoot = manipulator.ApplySpread(VECTOR_CONE_1DEGREES * 0.5, 1.0f);
	}
	else
	{*/
		vecShoot = manipulator.ApplySpread(VECTOR_CONE_4DEGREES, 1.0f);
	//}

	QAngle angShoot;
	VectorAngles(vecShoot, angShoot);

	CTaserDart *pDart = CTaserDart::DartCreate(vecSrc, angShoot, this);

	pDart->AddEffects(EF_NOSHADOW);

	vecShoot *= taser_dart_speed.GetFloat();

	pDart->Shoot(vecShoot/*, bStriderBuster*/);

	/*if (ShouldSeekTarget(pTargetEntity, bStriderBuster))
	{
		pDart->SetSeekTarget(pTargetEntity);
	}*/

	if (nShotNum == 1 && pTargetEntity->Classify() == CLASS_PLAYER_ALLY_VITAL)
	{
		// Make this person afraid and react to ME, not to the flechettes. 
		// Otherwise they could be scared into running towards the hunter.
		CSoundEnt::InsertSound(SOUND_DANGER | SOUND_CONTEXT_REACT_TO_SOURCE | SOUND_CONTEXT_EXCLUDE_COMBINE, pTargetEntity->EyePosition(), 180.0f, 2.0f, this);
	}

	return bClamped;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
bool CZombiePolice::CanPlantHere(const Vector &vecPos)
{
	// TODO: cache results?
	//if ( vecPos == m_vecLastCanPlantHerePos )
	//{
	//	return m_bLastCanPlantHere;
	//}

	Vector vecMins = GetHullMins();
	Vector vecMaxs = GetHullMaxs();

	vecMins.x -= 16;
	vecMins.y -= 16;

	vecMaxs.x += 16;
	vecMaxs.y += 16;
	vecMaxs.z -= zombie_police_plant_adjust_z.GetInt();

	bool bResult = false;

	trace_t tr;
	UTIL_TraceHull(vecPos, vecPos, vecMins, vecMaxs, MASK_NPCSOLID, this, COLLISION_GROUP_NONE, &tr);
	if (tr.startsolid)
	{
		// Try again, tracing down from above.
		Vector vecStart = vecPos;
		vecStart.z += zombie_police_plant_adjust_z.GetInt();

		UTIL_TraceHull(vecStart, vecPos, vecMins, vecMaxs, MASK_NPCSOLID, this, COLLISION_GROUP_NONE, &tr);
	}

	if (tr.startsolid)
	{
		//NDebugOverlay::Box( vecPos, vecMins, vecMaxs, 255, 0, 0, 0, 0 );
	}
	else
	{
		//NDebugOverlay::Box( vecPos, vecMins, vecMaxs, 0, 255, 0, 0, 0 );
		bResult = true;
	}

	// Cache the results in case we ask again for the same spot.	
	//m_vecLastCanPlantHerePos = vecPos;
	//m_bLastCanPlantHere = bResult;

	return bResult;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
int CZombiePolice::RangeAttack1Conditions(float flDot, float flDist)
{
	return COND_NONE;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
int CZombiePolice::RangeAttack2Conditions(float flDot, float flDist)
{
	//bool bIsBuster = IsStriderBuster(GetEnemy());
	bool bIsPerfectBullseye = (GetEnemy() && dynamic_cast<CNPC_Bullseye *>(GetEnemy()) && ((CNPC_Bullseye *)GetEnemy())->UsePerfectAccuracy());

	if (!bIsPerfectBullseye/* && !bIsBuster*/ && !taser_dart_test.GetBool() && (gpGlobals->curtime < m_flNextRangeAttack2Time))
	{
		return COND_NONE;
	}

	/*if (m_bDisableShooting)
	{
		return COND_NONE;
	}*/

	if (!HasCondition(COND_SEE_ENEMY))
	{
		return COND_NONE;
	}

	float flMaxFlechetteRange = taser_dart_max_range.GetFloat();

	/*if (IsUsingSiegeTargets())
	{
		flMaxFlechetteRange *= HUNTER_SIEGE_MAX_DIST_MODIFIER;
	}*/

	if (/*!bIsBuster &&*/ (flDist > flMaxFlechetteRange))
	{
		return COND_TOO_FAR_TO_ATTACK;
	}
	else if (/*!bIsBuster &&*/ (!GetEnemy() || !GetEnemy()->ClassMatches("npc_bullseye")) && flDist < taser_dart_min_range.GetFloat())
	{
		return COND_TOO_CLOSE_TO_ATTACK;
	}
	else if (flDot < ZOMBIE_POLICE_FACING_DOT)
	{
		return COND_NOT_FACING_ATTACK;
	}

	if (/*!bIsBuster &&*/ !m_bEnableUnplantedShooting && !taser_dart_test.GetBool() && !CanPlantHere(GetAbsOrigin()))
	{
		return COND_ZOMBIE_POLICE_CANT_PLANT;
	}

	return COND_CAN_RANGE_ATTACK2;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
bool CZombiePolice::WeaponLOSCondition(const Vector &ownerPos, const Vector &targetPos, bool bSetConditions)
{
	CBaseEntity *pTargetEnt;

	pTargetEnt = GetEnemy();

	trace_t tr;
	Vector vFrom = ownerPos + GetViewOffset();
	AI_TraceLine(vFrom, targetPos, MASK_SHOT, this, COLLISION_GROUP_NONE, &tr);

	if ((pTargetEnt && tr.m_pEnt == pTargetEnt) || tr.fraction == 1.0 || CanShootThrough(tr, targetPos))
	{
		static Vector vMins(-2.0, -2.0, -2.0);
		static Vector vMaxs(-vMins);
		// Hit the enemy, or hit nothing (traced all the way to a nonsolid enemy like a bullseye)
		AI_TraceHull(vFrom - Vector(0, 0, 18), targetPos, vMins, vMaxs, MASK_SHOT, this, COLLISION_GROUP_NONE, &tr);

		if ((pTargetEnt && tr.m_pEnt == pTargetEnt) || tr.fraction == 1.0 || CanShootThrough(tr, targetPos))
		{
			if (zombie_police_show_weapon_los_condition.GetBool())
			{
				NDebugOverlay::Line(vFrom, targetPos, 255, 0, 255, false, 0.1);
				NDebugOverlay::Line(vFrom - Vector(0, 0, 18), targetPos, 0, 0, 255, false, 0.1);
			}
			return true;
		}
	}
	else if (bSetConditions)
	{
		SetCondition(COND_WEAPON_SIGHT_OCCLUDED);
		SetEnemyOccluder(tr.m_pEnt);
	}

	return false;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
bool CZombiePolice::TestShootPosition(const Vector &vecShootPos, const Vector &targetPos)
{
	if (!CanPlantHere(vecShootPos))
	{
		return false;
	}

	return BaseClass::TestShootPosition(vecShootPos, targetPos);
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Vector CZombiePolice::Weapon_ShootPosition()
{
	matrix3x4_t gunMatrix;
	GetAttachment(gm_nTaserGunAttachment, gunMatrix);

	Vector vecShootPos;
	MatrixGetColumn(gunMatrix, 3, vecShootPos);

	return vecShootPos;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::MakeTracer(const Vector &vecTracerSrc, const trace_t &tr, int iTracerType)
{
	float flTracerDist;
	Vector vecDir;
	Vector vecEndPos;

	vecDir = tr.endpos - vecTracerSrc;

	flTracerDist = VectorNormalize(vecDir);

	int nAttachment = LookupAttachment("MiniGun");

	UTIL_Tracer(vecTracerSrc, tr.endpos, nAttachment, TRACER_FLAG_USEATTACHMENT, 5000, true, "HunterTracer");
}


//-----------------------------------------------------------------------------
// Trace didn't hit the intended target, but should the hunter
// shoot anyway? We use this to get the hunter to destroy 
// breakables that are between him and his target.
//-----------------------------------------------------------------------------
bool CZombiePolice::CanShootThrough(const trace_t &tr, const Vector &vecTarget)
{
	if (!tr.m_pEnt)
	{
		return false;
	}

	if (!tr.m_pEnt->GetHealth())
	{
		return false;
	}

	// Don't try to shoot through allies.
	CAI_BaseNPC *pNPC = tr.m_pEnt->MyNPCPointer();
	if (pNPC && (IRelationType(pNPC) == D_LI))
	{
		return false;
	}

	// Would a trace ignoring this entity continue to the target?
	trace_t continuedTrace;
	AI_TraceLine(tr.endpos, vecTarget, MASK_SHOT, tr.m_pEnt, COLLISION_GROUP_NONE, &continuedTrace);

	if (continuedTrace.fraction != 1.0)
	{
		if (continuedTrace.m_pEnt != GetEnemy())
		{
			return false;
		}
	}

	return true;
}

//-----------------------------------------------------------------------------
// Purpose: Sound of a footstep
//-----------------------------------------------------------------------------
void CZombiePolice::FootstepSound( bool fRightFoot )
{
	if( fRightFoot )
	{
		EmitSound( "NPC_MetroPolice.RunFootstepRight" );
	}
	else
	{
		EmitSound( "NPC_MetroPolice.RunFootstepLeft" );
	}
}

//-----------------------------------------------------------------------------
// Purpose: Sound of a foot sliding/scraping
//-----------------------------------------------------------------------------
void CZombiePolice::FootscuffSound( bool fRightFoot )
{
	if( fRightFoot )
	{
		EmitSound( "Zombie.ScuffRight" );
	}
	else
	{
		EmitSound( "Zombie.ScuffLeft" );
	}
}

//-----------------------------------------------------------------------------
// Purpose: Play a random attack hit sound
//-----------------------------------------------------------------------------
void CZombiePolice::AttackHitSound( void )
{
	EmitSound( "Zombie.AttackHit" );
}

//-----------------------------------------------------------------------------
// Purpose: Play a random attack miss sound
//-----------------------------------------------------------------------------
void CZombiePolice::AttackMissSound( void )
{
	// Play a random attack miss sound
	EmitSound( "Zombie.AttackMiss" );
}

//-----------------------------------------------------------------------------
// Purpose: 
//-----------------------------------------------------------------------------
void CZombiePolice::PainSound( const CTakeDamageInfo &info )
{
	// We're constantly taking damage when we are on fire. Don't make all those noises!
	if ( IsOnFire() )
	{
		return;
	}

	EmitSound( "Zombie.Pain" );
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::DeathSound( const CTakeDamageInfo &info ) 
{
	EmitSound( "Zombie.Die" );
}

//-----------------------------------------------------------------------------
// Purpose: 
//-----------------------------------------------------------------------------
void CZombiePolice::AlertSound( void )
{
	EmitSound( "Zombie.Alert" );

	// Don't let a moan sound cut off the alert sound.
	m_flNextMoanSound += random->RandomFloat( 2.0, 4.0 );
}

//-----------------------------------------------------------------------------
// Purpose: Returns a moan sound for this class of zombie.
//-----------------------------------------------------------------------------
const char *CZombiePolice::GetMoanSound( int nSound )
{
	return pMoanSounds[ nSound % ARRAYSIZE( pMoanSounds ) ];
}

//-----------------------------------------------------------------------------
// Purpose: Play a random idle sound.
//-----------------------------------------------------------------------------
void CZombiePolice::IdleSound( void )
{
	if( GetState() == NPC_STATE_IDLE && random->RandomFloat( 0, 1 ) == 0 )
	{
		// Moan infrequently in IDLE state.
		return;
	}

	if( IsSlumped() )
	{
		// Sleeping zombies are quiet.
		return;
	}

	EmitSound( "Zombie.Idle" );
	MakeAISpookySound( 360.0f );
}

//-----------------------------------------------------------------------------
// Purpose: Play a random attack sound.
//-----------------------------------------------------------------------------
void CZombiePolice::AttackSound( void )
{
	EmitSound( "Zombie.Attack" );
}

//-----------------------------------------------------------------------------
// Purpose: Returns the classname (ie "npc_headcrab") to spawn when our headcrab bails.
//-----------------------------------------------------------------------------
const char *CZombiePolice::GetHeadcrabClassname( void )
{
	return "npc_headcrab";
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
const char *CZombiePolice::GetHeadcrabModel( void )
{
	return "models/headcrabclassic.mdl";
}

//-----------------------------------------------------------------------------
// Purpose: 
//-----------------------------------------------------------------------------
const char *CZombiePolice::GetLegsModel( void )
{
	return "models/zombie/zombie_civ_police_legs.mdl";
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
const char *CZombiePolice::GetTorsoModel( void )
{
	return "models/zombie/zombie_civ_police_torso.mdl";
}


//---------------------------------------------------------
//---------------------------------------------------------
void CZombiePolice::SetZombieModel( void )
{
	Hull_t lastHull = GetHullType();

	if ( m_fIsTorso )
	{
		SetModel( "models/zombie/zombie_civ_police_torso.mdl" );
		SetHullType( HULL_TINY );
	}
	else
	{
		SetModel( "models/zombie/zombie_civ_police.mdl" );
		SetHullType( HULL_HUMAN );
	}

	SetBodygroup( ZOMBIE_BODYGROUP_HEADCRAB, !m_fIsHeadless );

	SetHullSizeNormal( true );
	SetDefaultEyeOffset();
	SetActivity( ACT_IDLE );

	// hull changed size, notify vphysics
	// UNDONE: Solve this generally, systematically so other
	// NPCs can change size
	if ( lastHull != GetHullType() )
	{
		if ( VPhysicsGetObject() )
		{
			SetupVPhysicsHull();
		}
	}
}

//---------------------------------------------------------
// Classic zombie only uses moan sound if on fire.
//---------------------------------------------------------
void CZombiePolice::MoanSound( envelopePoint_t *pEnvelope, int iEnvelopeSize )
{
	if( IsOnFire() )
	{
		BaseClass::MoanSound( pEnvelope, iEnvelopeSize );
	}
}

//---------------------------------------------------------
//---------------------------------------------------------
bool CZombiePolice::ShouldBecomeTorso( const CTakeDamageInfo &info, float flDamageThreshold )
{
	if( IsSlumped() ) 
	{
		// Never break apart a slouched zombie. This is because the most fun
		// slouched zombies to kill are ones sleeping leaning against explosive
		// barrels. If you break them in half in the blast, the force of being
		// so close to the explosion makes the body pieces fly at ridiculous 
		// velocities because the pieces weigh less than the whole.
		return false;
	}

	return BaseClass::ShouldBecomeTorso( info, flDamageThreshold );
}

//---------------------------------------------------------
//---------------------------------------------------------
void CZombiePolice::GatherConditions( void )
{
	BaseClass::GatherConditions();

	static int conditionsToClear[] = 
	{
		COND_BLOCKED_BY_DOOR,
		COND_DOOR_OPENED,
		COND_ZOMBIE_CHARGE_TARGET_MOVED,
	};

	ClearConditions( conditionsToClear, ARRAYSIZE( conditionsToClear ) );

	if ( m_hBlockingDoor == NULL || 
		 ( m_hBlockingDoor->m_toggle_state == TS_AT_TOP || 
		   m_hBlockingDoor->m_toggle_state == TS_GOING_UP )  )
	{
		ClearCondition( COND_BLOCKED_BY_DOOR );
		if ( m_hBlockingDoor != NULL )
		{
			SetCondition( COND_DOOR_OPENED );
			m_hBlockingDoor = NULL;
		}
	}
	else
		SetCondition( COND_BLOCKED_BY_DOOR );

	if ( ConditionInterruptsCurSchedule( COND_ZOMBIE_CHARGE_TARGET_MOVED ) )
	{
		if ( GetNavigator()->IsGoalActive() )
		{
			const float CHARGE_RESET_TOLERANCE = 60.0;
			if ( !GetEnemy() ||
				 ( m_vPositionCharged - GetEnemyLKP()  ).Length() > CHARGE_RESET_TOLERANCE )
			{
				SetCondition( COND_ZOMBIE_CHARGE_TARGET_MOVED );
			}
				 
		}
	}
}

//---------------------------------------------------------
//---------------------------------------------------------

int CZombiePolice::SelectFailSchedule( int failedSchedule, int failedTask, AI_TaskFailureCode_t taskFailCode )
{
	if ( HasCondition( COND_BLOCKED_BY_DOOR ) && m_hBlockingDoor != NULL )
	{
		ClearCondition( COND_BLOCKED_BY_DOOR );
		if ( m_NextTimeToStartDoorBash.Expired() && failedSchedule != SCHED_ZOMBIE_BASH_DOOR )
			return SCHED_ZOMBIE_BASH_DOOR;
		m_hBlockingDoor = NULL;
	}

	if ( failedSchedule != SCHED_ZOMBIE_CHARGE_ENEMY && 
		 IsPathTaskFailure( taskFailCode ) &&
		 random->RandomInt( 1, 100 ) < 50 )
	{
		return SCHED_ZOMBIE_CHARGE_ENEMY;
	}

	if ( failedSchedule != SCHED_ZOMBIE_WANDER_ANGRILY &&
		 ( failedSchedule == SCHED_TAKE_COVER_FROM_ENEMY || 
		   failedSchedule == SCHED_CHASE_ENEMY_FAILED ) )
	{
		return SCHED_ZOMBIE_WANDER_ANGRILY;
	}

	return BaseClass::SelectFailSchedule( failedSchedule, failedTask, taskFailCode );
}

//---------------------------------------------------------
//---------------------------------------------------------

int CZombiePolice::TranslateSchedule( int scheduleType )
{
	if ( scheduleType == SCHED_COMBAT_FACE && IsUnreachable( GetEnemy() ) )
		return SCHED_TAKE_COVER_FROM_ENEMY;

	if ( !m_fIsTorso && scheduleType == SCHED_FAIL )
		return SCHED_ZOMBIE_FAIL;

	return BaseClass::TranslateSchedule( scheduleType );
}

//---------------------------------------------------------

Activity CZombiePolice::NPC_TranslateActivity( Activity newActivity )
{
	newActivity = BaseClass::NPC_TranslateActivity( newActivity );

	if ( newActivity == ACT_RUN )
		return ACT_WALK;
		
	if ( m_fIsTorso && ( newActivity == ACT_ZOMBIE_POLICE_TANTRUM ) )
		return ACT_IDLE;
	if ((newActivity == ACT_WALK) || (newActivity == ACT_RUN))
	{
		if (GetEnemy())
		{
			Vector vecEnemyLKP = GetEnemyLKP();

			// Only start facing when we're close enough
			if (UTIL_DistApprox(vecEnemyLKP, GetAbsOrigin()) < ZOMBIE_POLICE_FACE_ENEMY_DIST)
			{
				return (Activity)ACT_ZOMBIE_POLICE_WALK_ANGRY;
			}
		}
	}
	else if ((newActivity == ACT_IDLE) && m_bPlanted)
	{
		return (Activity)ACT_ZOMBIE_POLICE_IDLE_PLANTED;
	}
	else if (newActivity == ACT_RANGE_ATTACK2)
	{
		if (!m_bPlanted && (m_bEnableUnplantedShooting /*|| IsStriderBuster(GetEnemy())*/))
		{
			return (Activity)ACT_ZOMBIE_POLICE_RANGE_ATTACK2_UNPLANTED;
		}
	}

	return newActivity;
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::InputEnableUnplantedShooting(inputdata_t &inputdata)
{
	m_bEnableUnplantedShooting = true;
}


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void CZombiePolice::InputDisableUnplantedShooting(inputdata_t &inputdata)
{
	m_bEnableUnplantedShooting = false;
}

//---------------------------------------------------------
//---------------------------------------------------------
void CZombiePolice::OnStateChange( NPC_STATE OldState, NPC_STATE NewState )
{
	BaseClass::OnStateChange( OldState, NewState );
}

//---------------------------------------------------------
//---------------------------------------------------------

void CZombiePolice::StartTask( const Task_t *pTask )
{
	switch( pTask->iTask )
	{
	case TASK_ZOMBIE_EXPRESS_ANGER:
		{
			if ( random->RandomInt( 1, 4 ) == 2 )
			{
				SetIdealActivity( (Activity)ACT_ZOMBIE_POLICE_TANTRUM );
			}
			else
			{
				TaskComplete();
			}

			break;
		}

	case TASK_ZOMBIE_YAW_TO_DOOR:
		{
			AssertMsg( m_hBlockingDoor != NULL, "Expected condition handling to break schedule before landing here" );
			if ( m_hBlockingDoor != NULL )
			{
				GetMotor()->SetIdealYaw( m_flDoorBashYaw );
			}
			TaskComplete();
			break;
		}

	case TASK_ZOMBIE_ATTACK_DOOR:
		{
		 	m_DurationDoorBash.Reset();
			SetIdealActivity( SelectDoorBash() );
			break;
		}

	case TASK_ZOMBIE_CHARGE_ENEMY:
		{
			if ( !GetEnemy() )
				TaskFail( FAIL_NO_ENEMY );
			else if ( GetNavigator()->SetVectorGoalFromTarget( GetEnemy()->GetLocalOrigin() ) )
			{
				m_vPositionCharged = GetEnemy()->GetLocalOrigin();
				TaskComplete();
			}
			else
				TaskFail( FAIL_NO_ROUTE );
			break;
		}

	case TASK_ZOMBIE_POLICE_PRE_RANGE_ATTACK2:
	{
		if (!(m_bPlanted)) //(!m_bPlanted || (GetEnemy() && IsStriderBuster(GetEnemy())))
		{
			TaskComplete();
		}
		else
		{
			SetIdealActivity(ACT_ZOMBIE_POLICE_TANTRUM);
		}
		break;
	}

	case TASK_ZOMBIE_POLICE_SHOOT_COMMIT:
	{
		// We're committing to shooting. Don't allow interrupts until after we've shot a bit (see TASK_RANGE_ATTACK1).
		m_flShootAllowInterruptTime = gpGlobals->curtime + 100.0f;
		TaskComplete();
		break;
	}

	case TASK_RANGE_ATTACK2:
	{
		if (GetEnemy())
		{

			SetIdealActivity(ACT_RANGE_ATTACK2);

			// Decide how many shots to fire.
			int nShots = taser_dart_volley_size.GetInt();
			if (g_pGameRules->IsSkillLevel(SKILL_EASY))
			{
				nShots--;
			}

			// Decide when to fire the first shot.
			float initialDelay = taser_first_dart_delay.GetFloat();
			/*if (bIsBuster)
			{
				initialDelay = 0; //*= 0.5;
			}*/

			BeginVolley(nShots, gpGlobals->curtime + initialDelay);

			// In case we need to miss on purpose, pick a direction now.
			m_bMissLeft = false;
			if (random->RandomInt(0, 1) == 0)
			{
				m_bMissLeft = true;
			}

			//LockBothEyes(initialDelay + (nShots * taser_dart_delay.GetFloat()));
		}
		else
		{
			TaskFail(FAIL_NO_ENEMY);
		}

		break;
	}

	default:
		BaseClass::StartTask( pTask );
		break;
	}
}

//---------------------------------------------------------
//---------------------------------------------------------

void CZombiePolice::RunTask( const Task_t *pTask )
{
	switch( pTask->iTask )
	{
	case TASK_ZOMBIE_ATTACK_DOOR:
		{
			if ( IsActivityFinished() )
			{
				if ( m_DurationDoorBash.Expired() )
				{
					TaskComplete();
					m_NextTimeToStartDoorBash.Reset();
				}
				else
					ResetIdealActivity( SelectDoorBash() );
			}
			break;
		}

	case TASK_ZOMBIE_CHARGE_ENEMY:
		{
			break;
		}

	case TASK_ZOMBIE_EXPRESS_ANGER:
		{
			if ( IsActivityFinished() )
			{
				TaskComplete();
			}
			break;
		}

	default:
		BaseClass::RunTask( pTask );
		break;
	}
}

//---------------------------------------------------------
//---------------------------------------------------------

bool CZombiePolice::OnObstructingDoor( AILocalMoveGoal_t *pMoveGoal, CBaseDoor *pDoor, 
							  float distClear, AIMoveResult_t *pResult )
{
	if ( BaseClass::OnObstructingDoor( pMoveGoal, pDoor, distClear, pResult ) )
	{
		if  ( IsMoveBlocked( *pResult ) && pMoveGoal->directTrace.vHitNormal != vec3_origin )
		{
			m_hBlockingDoor = pDoor;
			m_flDoorBashYaw = UTIL_VecToYaw( pMoveGoal->directTrace.vHitNormal * -1 );	
		}
		return true;
	}

	return false;
}

//---------------------------------------------------------
//---------------------------------------------------------

Activity CZombiePolice::SelectDoorBash()
{
	if ( random->RandomInt( 1, 3 ) == 1 )
		return ACT_MELEE_ATTACK1;
	return (Activity)ACT_ZOMBIE_POLICE_WALLPOUND;
}

//---------------------------------------------------------
// Zombies should scream continuously while burning, so long
// as they are alive... but NOT IN GERMANY!
//---------------------------------------------------------
void CZombiePolice::Ignite( float flFlameLifetime, bool bNPCOnly, float flSize, bool bCalledByLevelDesigner )
{
 	if( !IsOnFire() && IsAlive() )
	{
		BaseClass::Ignite( flFlameLifetime, bNPCOnly, flSize, bCalledByLevelDesigner );

		if ( !UTIL_IsLowViolence() )
		{
			RemoveSpawnFlags( SF_NPC_GAG );

			MoanSound( envZombiePoliceMoanIgnited, ARRAYSIZE( envZombiePoliceMoanIgnited ) );

			if ( m_pMoanSound )
			{
				ENVELOPE_CONTROLLER.SoundChangePitch( m_pMoanSound, 120, 1.0 );
				ENVELOPE_CONTROLLER.SoundChangeVolume( m_pMoanSound, 1, 1.0 );
			}
		}
	}
}

//---------------------------------------------------------
// If a zombie stops burning and hasn't died, quiet him down
//---------------------------------------------------------
void CZombiePolice::Extinguish()
{
	if( m_pMoanSound )
	{
		ENVELOPE_CONTROLLER.SoundChangeVolume( m_pMoanSound, 0, 2.0 );
		ENVELOPE_CONTROLLER.SoundChangePitch( m_pMoanSound, 100, 2.0 );
		m_flNextMoanSound = gpGlobals->curtime + random->RandomFloat( 2.0, 4.0 );
	}

	BaseClass::Extinguish();
}

//---------------------------------------------------------
//---------------------------------------------------------
int CZombiePolice::OnTakeDamage_Alive( const CTakeDamageInfo &inputInfo )
{
#ifndef HL2_EPISODIC
	if ( inputInfo.GetDamageType() & DMG_BUCKSHOT )
	{
		if( !m_fIsTorso && inputInfo.GetDamage() > (m_iMaxHealth/3) )
		{
			// Always flinch if damaged a lot by buckshot, even if not shot in the head.
			// The reason for making sure we did at least 1/3rd of the zombie's max health
			// is so the zombie doesn't flinch every time the odd shotgun pellet hits them,
			// and so the maximum number of times you'll see a zombie flinch like this is 2.(sjb)
			AddGesture( ACT_GESTURE_FLINCH_HEAD );
		}
	}
#endif // HL2_EPISODIC

	return BaseClass::OnTakeDamage_Alive( inputInfo );
}

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
bool CZombiePolice::IsHeavyDamage( const CTakeDamageInfo &info )
{
#ifdef HL2_EPISODIC
	if ( info.GetDamageType() & DMG_BUCKSHOT )
	{
		if ( !m_fIsTorso && info.GetDamage() > (m_iMaxHealth/3) )
			return true;
	}

	// Randomly treat all damage as heavy
	if ( info.GetDamageType() & (DMG_BULLET | DMG_BUCKSHOT) )
	{
		// Don't randomly flinch if I'm melee attacking
		if ( !HasCondition(COND_CAN_MELEE_ATTACK1) && (RandomFloat() > 0.5) )
		{
			// Randomly forget I've flinched, so that I'll be forced to play a big flinch
			// If this doesn't happen, it means I may not fully flinch if I recently flinched
			if ( RandomFloat() > 0.75 )
			{
				Forget(bits_MEMORY_FLINCHED);
			}

			return true;
		}
	}
#endif // HL2_EPISODIC

	return BaseClass::IsHeavyDamage(info);
}

//---------------------------------------------------------
//---------------------------------------------------------
#define ZOMBIE_SQUASH_MASS	300.0f  // Anything this heavy or heavier squashes a zombie good. (show special fx)
bool CZombiePolice::IsSquashed( const CTakeDamageInfo &info )
{
	if( GetHealth() > 0 )
	{
		return false;
	}

	if( info.GetDamageType() & DMG_CRUSH )
	{
		IPhysicsObject *pCrusher = info.GetInflictor()->VPhysicsGetObject();
		if( pCrusher && pCrusher->GetMass() >= ZOMBIE_SQUASH_MASS && info.GetInflictor()->WorldSpaceCenter().z > EyePosition().z )
		{
			// This heuristic detects when a zombie has been squashed from above by a heavy
			// item. Done specifically so we can add gore effects to Ravenholm cartraps.
			// The zombie must take physics damage from a 300+kg object that is centered above its eyes (comes from above)
			return true;
		}
	}

	return false;
}

//---------------------------------------------------------
//---------------------------------------------------------
void CZombiePolice::BuildScheduleTestBits( void )
{
	BaseClass::BuildScheduleTestBits();

	// Our range attack is uninterruptable for the first few seconds.
	if (IsCurSchedule(SCHED_ZOMBIE_POLICE_RANGE_ATTACK2, false) && (gpGlobals->curtime < m_flShootAllowInterruptTime))
	{
		ClearCustomInterruptConditions();
		SetCustomInterruptCondition(COND_HEAVY_DAMAGE);
	}
	else if (IsCurSchedule(SCHED_ZOMBIE_POLICE_RANGE_ATTACK2, false) && (GetActivity() == ACT_TRANSITION))
	{
		// Don't stop unplanting just because we can range attack again.
		ClearCustomInterruptCondition(COND_CAN_RANGE_ATTACK1);
		ClearCustomInterruptCondition(COND_CAN_RANGE_ATTACK2);
	}

	if( !m_fIsTorso && !IsCurSchedule( SCHED_FLINCH_PHYSICS ) && !m_ActBusyBehavior.IsActive() )
	{
		SetCustomInterruptCondition( COND_PHYSICS_DAMAGE );
	}
}

	
//=============================================================================

AI_BEGIN_CUSTOM_NPC( npc_zombie, CZombiePolice )

	DECLARE_CONDITION( COND_BLOCKED_BY_DOOR )
	DECLARE_CONDITION( COND_DOOR_OPENED )
	DECLARE_CONDITION( COND_ZOMBIE_CHARGE_TARGET_MOVED )

	DECLARE_TASK( TASK_ZOMBIE_EXPRESS_ANGER )
	DECLARE_TASK( TASK_ZOMBIE_YAW_TO_DOOR )
	DECLARE_TASK( TASK_ZOMBIE_ATTACK_DOOR )
	DECLARE_TASK( TASK_ZOMBIE_CHARGE_ENEMY )
	DECLARE_TASK( TASK_ZOMBIE_POLICE_PRE_RANGE_ATTACK2 )
	DECLARE_TASK( TASK_ZOMBIE_POLICE_SHOOT_COMMIT )
	
	DECLARE_ACTIVITY( ACT_ZOMBIE_POLICE_TANTRUM );
	DECLARE_ACTIVITY( ACT_ZOMBIE_POLICE_WALLPOUND );
	DECLARE_ACTIVITY( ACT_ZOMBIE_POLICE_WALK_ANGRY );
	DECLARE_ACTIVITY( ACT_ZOMBIE_POLICE_RANGE_ATTACK2_UNPLANTED );
	DECLARE_ACTIVITY( ACT_ZOMBIE_POLICE_IDLE_PLANTED );

	DEFINE_SCHEDULE
	( 
		SCHED_ZOMBIE_BASH_DOOR,

		"	Tasks"
		"		TASK_SET_ACTIVITY				ACTIVITY:ACT_ZOMBIE_POLICE_TANTRUM"
		"		TASK_SET_FAIL_SCHEDULE			SCHEDULE:SCHED_TAKE_COVER_FROM_ENEMY"
		"		TASK_ZOMBIE_YAW_TO_DOOR			0"
		"		TASK_FACE_IDEAL					0"
		"		TASK_ZOMBIE_ATTACK_DOOR			0"
		""
		"	Interrupts"
		"		COND_ZOMBIE_RELEASECRAB"
		"		COND_ENEMY_DEAD"
		"		COND_NEW_ENEMY"
		"		COND_DOOR_OPENED"
	)

	DEFINE_SCHEDULE
	(
		SCHED_ZOMBIE_WANDER_ANGRILY,

		"	Tasks"
		"		TASK_WANDER						480240" // 48 units to 240 units.
		"		TASK_WALK_PATH					0"
		"		TASK_WAIT_FOR_MOVEMENT			4"
		""
		"	Interrupts"
		"		COND_ZOMBIE_RELEASECRAB"
		"		COND_ENEMY_DEAD"
		"		COND_NEW_ENEMY"
		"		COND_DOOR_OPENED"
	)

	DEFINE_SCHEDULE
	(
		SCHED_ZOMBIE_CHARGE_ENEMY,


		"	Tasks"
		"		TASK_ZOMBIE_CHARGE_ENEMY		0"
		"		TASK_WALK_PATH					0"
		"		TASK_WAIT_FOR_MOVEMENT			0"
		"		TASK_PLAY_SEQUENCE				ACTIVITY:ACT_ZOMBIE_POLICE_TANTRUM" /* placeholder until frustration/rage/fence shake animation available */
		""
		"	Interrupts"
		"		COND_ZOMBIE_RELEASECRAB"
		"		COND_ENEMY_DEAD"
		"		COND_NEW_ENEMY"
		"		COND_DOOR_OPENED"
		"		COND_ZOMBIE_CHARGE_TARGET_MOVED"
	)

	DEFINE_SCHEDULE
	(
		SCHED_ZOMBIE_FAIL,

		"	Tasks"
		"		TASK_STOP_MOVING		0"
		"		TASK_SET_ACTIVITY		ACTIVITY:ACT_ZOMBIE_POLICE_TANTRUM"
		"		TASK_WAIT				1"
		"		TASK_WAIT_PVS			0"
		""
		"	Interrupts"
		"		COND_CAN_RANGE_ATTACK1 "
		"		COND_CAN_RANGE_ATTACK2 "
		"		COND_CAN_MELEE_ATTACK1 "
		"		COND_CAN_MELEE_ATTACK2"
		"		COND_GIVE_WAY"
		"		COND_DOOR_OPENED"
	)

	//=========================================================
	// Attack (Deploy/shoot/finish)
	//=========================================================
	DEFINE_SCHEDULE
	(
	SCHED_ZOMBIE_POLICE_RANGE_ATTACK2,

	"	Tasks"
	"		TASK_STOP_MOVING				0"
	"		TASK_ZOMBIE_POLICE_PRE_RANGE_ATTACK2	0"
	"		TASK_ZOMBIE_POLICE_SHOOT_COMMIT		0"
	"		TASK_RANGE_ATTACK2				0"
	//"		TASK_ZOMBIE_POLICE_FINISH_RANGE_ATTACK	0"
	"		TASK_SET_ACTIVITY				ACTIVITY:ACT_IDLE"
	"		TASK_WAIT        				0.4"
	"		TASK_WAIT_RANDOM				0.2"
	"	"
	"	Interrupts"
	"		COND_NEW_ENEMY"
	)

AI_END_CUSTOM_NPC()

//=============================================================================
