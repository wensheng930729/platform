package com.bee.platform.user.dao.mapper;

import java.util.List;
import java.util.Map;

import org.apache.ibatis.annotations.Select;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.user.dto.EnterprisesCountDTO;
import com.bee.platform.user.dto.ManagerSearchParamDTO;
import com.bee.platform.user.dto.UserManagementDTO;
import com.bee.platform.user.entity.Enterprises;
import com.bee.platform.user.entity.EnterprisesUsers;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface EnterprisesUsersMapper extends BaseMapper<EnterprisesUsers> {
    
    @Select("select e.* from enterprises e left join enterprises_users eu on e.id = eu.enterprise_id "
        + "where eu.user_id = #{userId}")
    List<Enterprises> listByUserId(int userId);
    /**
     * @notes 通过用户id查询用户所在的企业id
     * @Author junyang.li
     * @Date 11:35 2019/3/18
     **/
    List<Integer> listOrgIdsByUserId(int userId);
    /**
     * @notes 查询企业下的成员数量
     * @Author junyang.li
     * @Date 13:43 2019/3/18
     **/
    List<EnterprisesCountDTO>  countUserByOrgIds(List<Integer> orgId);
    /**
     * @notes  通过企业id查询用户id
     * @Author junyang.li
     * @Date 13:48 2019/3/20
     **/
    List<UserManagementDTO> selectUserIdsByOrgId(ManagerSearchParamDTO dto, Pagination pagination);

    /**
     * @notes 每天凌晨重置用户被邀请的状态
     * @param list EnterprisesUsers表主键
     * @Author junyang.li
     * @Date 16:30 2019/3/20
     **/
    void updateInvite(List<Integer> list);
    
    @Select("SELECT phone FROM enterprises_users where phone = #{phone}")
    String selectEnterprisesUsersPhone(String phone);
    
    //@Select("SELECT * FROM enterprises_users WHERE  (phone like CONCAT('%', #{nameOrPhone}, '%') or nickname like CONCAT('%', #{nameOrPhone}, '%')) and zpostid = #{zpostid} and departmentsid = #{departmentsid}")
    List<EnterprisesUsers> selectEnterprisesUsers(Map<String,Object> paramMap ,Pagination pagination);
    
    @Select("select * from enterprises_users where user_id = #{userId}")
    EnterprisesUsers selectEnterprisesUsersByUserId(int userId);
    
   // @Select("select * from enterprises_users where enterprise_id = #{enterpriseId}")
    List<EnterprisesUsers> selectByEnterpriseId(Integer enterpriseId,Pagination pagination);
}
