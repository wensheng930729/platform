package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.authority.dto.AuthEnterpriseRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthRoleUsedDTO;
import com.bee.platform.user.authority.dto.EnterpriseUrlDTO;
import com.bee.platform.user.authority.entity.AuthEnterpriseRole;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 企业与角色（角色或功能的关联表）的中间表 Mapper 接口
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
public interface AuthEnterpriseRoleMapper extends BaseMapper<AuthEnterpriseRole> {

    /**
     * 查询企业下的所有应用
     *
     * @param enterpriseId 企业id
     * @return 企业下的所有应用
     */
    List<AuthEnterpriseRoleTreeDTO> getEnterpriseRole(Integer enterpriseId);

    /**
     * 查询企业下的所有基础角色
     *
     * @param funTwoIds 二级功能id集合
     * @return 企业下的所有基础角色
     */
    List<AuthEnterpriseRoleTreeDTO> getEnterpriseBaseRole(List<Integer> funTwoIds);

    List<AuthEnterpriseRoleTreeDTO> getEnterpriseAppRole(List<Integer> appIds);


    /**
     * @param list :
     * @notes: 批量插入企业角色的关联数据
     * @Author: junyang.li
     * @Date: 16:04 2019/5/24
     * @return: void
     */
    void insertAll(List<AuthEnterpriseRole> list);


    /**
     * 查询企业下的url
     *
     * @param enterpriseId 企业id
     * @param subSys       子系统标识
     * @return 企业下的url
     */
    List<EnterpriseUrlDTO> getEnterpriseInterfaceUri(@Param("enterpriseId") Integer enterpriseId, @Param("subSys") String subSys);

    /**
     * 根据企业id查询 企业开通的角色
     *
     * @param id
     * @return
     */
    List<AuthRoleUsedDTO> getEnterpriseFunsByEnterpriseId(@Param("id") Integer id);
}
