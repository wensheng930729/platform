package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.bee.platform.user.dto.EnterpriseDetailDTO;
import com.bee.platform.user.entity.Enterprises;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface EnterprisesMapper extends BaseMapper<Enterprises> {
    @Select("select e.* from enterprises e  "
            + "where e.name = #{enterpriseName}")
    List<EnterpriseDetailDTO> listByEnterpriseName(String enterpriseName);
}
