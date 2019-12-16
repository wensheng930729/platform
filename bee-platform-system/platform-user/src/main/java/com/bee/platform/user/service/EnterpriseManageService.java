package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.business.dto.AllCheckTypeDTO;
import com.bee.platform.business.dto.EnterprisesWithAppDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.entity.AuthEnterprise;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-05-07
 */
public interface EnterpriseManageService extends IService<AuthEnterprise> {


    /**
     * 获取审核结果类型及对应企业数
     * @param managerInfo
     * @return
     */
    ResponseResult<AllCheckTypeDTO> getTypeWithCount(AuthPlatformUserInfo managerInfo);

    /**
     * 条件查询企业审核列表
     * @param managerInfo
     * @param typeId
     * @param name
     * @param page
     * @return
     */
    ResponseResult<List<EnterprisesWithAppDTO>> getEnterpriseList(AuthPlatformUserInfo managerInfo, Integer typeId, String name, Page page);

    /**
     * @notes: 企业管理员重置企业用户密码
     * @Author: junyang.li
     * @Date: 14:54 2019/5/9
     * @param userInfo : 操作人信息
     * @param userId : 被操作人id
     * @param type : 通知类型
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    ResponseResult<ResCodeEnum> resetPassword(AuthPlatformUserInfo userInfo,Integer userId,Integer type);

    /**
     * 取消认证
     * @param checkId
     * @param userInfo
     * @return
     */
	ResponseResult cancelApply(Integer checkId, AuthPlatformUserInfo userInfo);
}
