package com.bee.platform.user.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.user.entity.EnterprisesCheck;
import com.baomidou.mybatisplus.mapper.BaseMapper;
import org.springframework.data.repository.query.Param;

import java.util.List;

/**
 * <p>
 *  Mapper 接口
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface EnterprisesCheckMapper extends BaseMapper<EnterprisesCheck> {

    /**
     * 查询用户申请认证还未通过的企业信息
     * @param list
     * @return
     */
    List<EnterprisesCheck> findByTypes(List<Integer> list, String phone);

    /**
     * 分页查询所有企业审核信息
     * @param pagination
     * @return
     */
    List<EnterprisesCheck> getAllEnterpriseCheckByPage(Pagination pagination);

    /**
     * 获取未处理的企业数量（我的待办数量）、已通过企业数量（已通过企业）、申请企业总数（申请总数）
     * @param list
     * @return
     */
    Long getCounts(List list);

}
