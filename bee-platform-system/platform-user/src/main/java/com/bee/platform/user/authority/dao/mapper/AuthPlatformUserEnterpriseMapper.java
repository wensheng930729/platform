package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.authority.dto.AuthEnterpriseDepartmentPostDto;
import com.bee.platform.user.authority.dto.AuthPlatformUserDto;
import com.bee.platform.user.authority.entity.AuthPlatformUserEnterprise;
import com.bee.platform.user.dto.EnterprisesCountDTO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 企业与用户中间表 Mapper 接口
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
public interface AuthPlatformUserEnterpriseMapper extends BaseMapper<AuthPlatformUserEnterprise> {
    
    /**
     * @Description 批量添加用户管理企业
     * @Param null
     * @Date 2019/5/23 14:12
     * @Author xin.huang
     * @Return
     */
    int batchInsert(List<AuthPlatformUserEnterprise> list);

    /**
     * @Description 查询用户所在企业的信息详情
     * @Param userId
     * @Param enterpriseId
     * @Date 2019/5/27 14:03
     * @Author xin.huang
     * @Return
     */
    AuthPlatformUserDto findUserEnterpriseInfo(@Param("userId") Integer userId, @Param("enterpriseId")Integer enterpriseId);

    /**
     * @Description 查询用户所在公司部门职位信息
     * @Param userId
     * @Date 2019/5/27 17:42
     * @Author xin.huang
     * @Return
     */
    List<AuthEnterpriseDepartmentPostDto> findUserDepartmentAndPostInfo(@Param("userId") Integer userId, @Param("enterpriseIds") List<Integer> enterpriseIds);
    /**
     * @notes 查询企业下的成员数量
     * @Author junyang.li
     * @Date 13:43 2019/3/18
     **/
    List<EnterprisesCountDTO>  countUserByOrgIds(List<Integer> orgId);

    /**
     * 查询用户所关联的企业id
     * @param userId
     * @return
     */
    List<Integer> findUserEnterpriseIds(@Param("userId") Integer userId);
}
