package com.bee.platform.user.dao.mapper;

import com.bee.platform.user.entity.EnterprisesRelationUserCheckLog;

import java.util.List;

import org.apache.ibatis.annotations.Select;

import com.baomidou.mybatisplus.mapper.BaseMapper;

/**
 * <p>
 * 企业关联用户审核日志表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */
public interface EnterprisesRelationUserCheckLogMapper extends BaseMapper<EnterprisesRelationUserCheckLog> {
	@Select("SELECT * FROM enterprises_relation_user_check_log WHERE enterprise_relation_user_check_id = #{enterpriseRelationUserCheckId}")
	List<EnterprisesRelationUserCheckLog> selectEnterprisesRelationUserCheckLog(int enterpriseRelationUserCheckId);
}
