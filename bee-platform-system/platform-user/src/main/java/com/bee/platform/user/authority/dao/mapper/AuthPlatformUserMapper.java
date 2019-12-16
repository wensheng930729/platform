package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.rq.AuthPlatformUserOneInRQ;
import com.bee.platform.user.authority.rq.AuthPlatformUserSelectINRQ;
import com.bee.platform.user.authority.rq.AuthPlatformUserSelectRQ;
import org.apache.ibatis.annotations.Param;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author xin.huang
 * @since 2019-05-20
 */
public interface AuthPlatformUserMapper extends BaseMapper<AuthPlatformUser> {

    /**
     * @Description 条件查询用户
     * @Param authPlatformUserSelectRQ
     * @Param pagination
     * @Date 2019/5/21 10:32
     * @Author xin.huang
     * @Return
     */
     List<AuthPlatformUserDto> findList(AuthPlatformUserSelectRQ authPlatformUserSelectRQ, Pagination pagination);

    /**
     * @notes: 批量插入用户
     * @Author: junyang.li
     * @Date: 10:51 2019/5/24
     * @param list : 用户集合
     * @return: void
     */
    void insertAllUser(List<AuthPlatformUser> list);
    
    List<AuthPlatformUserModifyDto> selectByMap(Map<String,Object> paramMap,Pagination pagination);

    List<AuthPlatformUserModifyDto> selectMap(Pagination pagination);

	ArrayList<AuthPlatformUserModifyInDto> queryIn(AuthPlatformUserSelectINRQ authPlatformUserSelectINRQ, Pagination pagination);
	
	AuthPlatformUserOneInDTO queryInOne(AuthPlatformUserOneInRQ authPlatformUserOneInRQ);

    /**
     * 通过关键词查询当前企业的用户信息
     * @param enterpriseId 当前企业
     * @param keyword 关键词
     * @param pagination 分页对象
     * @return 用户列表
     */
	List<AuthPlatformUser> selectUserByKeyword(@Param("enterpriseId")Integer enterpriseId,@Param("keyword")String keyword,Pagination pagination);

    List<AuthPlatformUserDto> listByenterpriseId(AuthPlatformUserSelectRQ rq, Pagination pagination);

    List<AuthPlatformUserDto> findEnterpriseUsers(@Param("ids") List<Integer> ids);

    /**
     * 获取用户公司所有员工-下拉列表
     * @param orgId
     * @return
     */
    List<AuthPlatformUserPullDownDto> getAllCompanyUserById(Integer orgId);
}
