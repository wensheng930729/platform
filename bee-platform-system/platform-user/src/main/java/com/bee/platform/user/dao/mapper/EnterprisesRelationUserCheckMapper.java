package com.bee.platform.user.dao.mapper;

import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.user.dto.EnterprisesRelationUserCheckDTO;
import com.bee.platform.user.entity.EnterprisesRelationUserCheck;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.ibatis.annotations.Select;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;

/**
 * <p>
 * 企业关联用户审核表 Mapper 接口
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-05
 */
public interface EnterprisesRelationUserCheckMapper extends BaseMapper<EnterprisesRelationUserCheck> {
	
	//@Select("SELECT * FROM enterprises_relation_user_check WHERE  (phone like CONCAT('%', #{nameOrPhone}, '%') or nickname like CONCAT('%', #{nameOrPhone}, '%')) and createTime>#{startTime} and createTime<#{endTime}")
	List<EnterprisesRelationUserCheck> selectEnterprisesRelationUserCheck(Map<String,Object> paramMap,Pagination pagination);
	
	ArrayList<EnterprisesRelationUserCheck> getAllApplyList(Map<String,Object> map,Pagination pagination);
}
