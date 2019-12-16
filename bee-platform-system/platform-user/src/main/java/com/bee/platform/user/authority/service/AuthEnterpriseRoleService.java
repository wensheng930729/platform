package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthEnterpriseRoleTreeDTO;
import com.bee.platform.user.authority.dto.EnterpriseUrlDTO;
import com.bee.platform.user.authority.entity.AuthEnterpriseRole;

import java.util.List;

/**
 * <p>
 * 企业与角色（角色或功能的关联表）的中间表 服务类
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
public interface AuthEnterpriseRoleService extends IService<AuthEnterpriseRole> {


    /**
     * 查询企业下所有应用树
     * @param enterpriseId 企业id
     * @return  企业下所有应用树
     */
    ResponseResult<List<AuthEnterpriseRoleTreeDTO>> getEnterpriseRoleTreeList(Integer enterpriseId);
    /**
     * @notes: 批量插入企业角色的关联数据
     * @Author: junyang.li
     * @Date: 16:01 2019/5/24
     * @param enterpriseRoles :
     * @return: void
     */
    void insertAll(List<AuthEnterpriseRole> enterpriseRoles);

    /**
     * 获取企业下的接口url
     * @param enterpriseId 企业id
     * @param subSys  系统标识
     * @return 企业下的url
     */
    List<EnterpriseUrlDTO> getEnterpriseInterfaceUri(Integer enterpriseId, String subSys);


}
