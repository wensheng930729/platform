package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.dto.EnterprisesUsersDTO;
import com.bee.platform.user.dto.EnterprisesUsersInfoDTO;
import com.bee.platform.user.dto.EnterprisesUsersListDTO;
import com.bee.platform.user.dto.EnterprisesUsersRQ;
import com.bee.platform.user.entity.EnterprisesUsers;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface EnterprisesUsersService extends IService<EnterprisesUsers> {

    EnterprisesUsers findByUserIdAndEnterpriseId(Integer userId, Integer enterpriseId);
    
//    List<AuthEnterprise> listByUserId(Integer userId);

    ResponseResult<EnterprisesUsersDTO> getEnterpriseUserInfoById(Integer userId, Integer enterpriseId);

    /**
     * @notes 每天凌晨重置用户被邀请的状态
     * @param ids EnterprisesUsers表主键
     * @Author junyang.li
     * @Date 16:30 2019/3/20
     **/
    void updateInvite(List<Integer> ids);
    /**
     * @notes 查询该用户的企业id
     * @Author junyang.li
     * @Date 17:07 2019/1/18
     **/
    List<Integer> userInEnterprises(Integer userId,List<Integer> orgIds);

	ResponseResult<Object> add(EnterprisesUsersInfoDTO enterprisesUsersInfoDTO, AuthPlatformUserInfo userInfo);

	List<EnterprisesUsersListDTO> queryAll(AuthPlatformUserInfo userInfo,Pagination pagination);
	
	ResponseResult<Object> upDateById(int id, Integer isActive,AuthPlatformUserInfo userInfo);

	ResponseResult<Object> upDateByIdEnterprisesUsers(AuthPlatformUserInfo userInfo, EnterprisesUsersInfoDTO enterprisesUsersInfoDTO);

	ResponseResult<Object> getEnterprisesUsers(String phone);

	ResponseResult<EnterprisesUsersInfoDTO> getEnterprisesUsersById(Integer id);

	ResponseResult<ArrayList<Object>> query(AuthPlatformUserInfo userInfo, EnterprisesUsersRQ rq, Pagination pagination);

}
