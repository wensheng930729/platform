package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.dto.EnterprisesUserDTO;
import com.bee.platform.user.entity.User;

import java.util.Date;
import java.util.List;

import org.apache.ibatis.annotations.Select;

/**
 * <p>
 * 用户表（主体） Mapper 接口
 * </p>
 *
 * @author junyang.li123
 * @since 2018-12-25
 */
public interface UserMapper extends BaseMapper<User> {

    void updateByParam(User user);

    void insertAll(List<User> list);

    List<Long> selectByUserName(List<String> userNames);

    void expiresToken(String financeToken, Date time);

    /**
     * 根据企业id查询用户
     * @param orgId
     * @return
     */
    List<User> selectUsersByEnterpriseId(Integer orgId);
    
    @Select("SELECT phone FROM users where phone = #{phone}")
    String selectUserByPhone(String phone);

	EnterprisesUserDTO getAdminByOrgId(Integer orgId);

	List<EnterprisesUserDTO> getUserByOrgId(Integer orgId);

}
