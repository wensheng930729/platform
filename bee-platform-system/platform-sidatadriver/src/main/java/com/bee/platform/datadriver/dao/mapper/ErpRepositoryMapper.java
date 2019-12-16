package com.bee.platform.datadriver.dao.mapper;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.datadriver.dto.ErpRepositoryListDTO;
import com.bee.platform.datadriver.entity.ErpRepository;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 仓库档案 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpRepositoryMapper extends BaseMapper<ErpRepository> {
    /**
     * 仓库列表
     * @param pagination
     * @param orgId
     * @param status
     * @return
     */
    public List<ErpRepositoryListDTO> query(Pagination pagination, @Param("orgId") Integer orgId,@Param("status") Integer status);
}
