package com.bee.platform.user.authority.dao.mapper;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.user.authority.entity.TSystemCodeT;
import com.baomidou.mybatisplus.mapper.BaseMapper;

import java.util.List;
import java.util.Map;

/**
 * <p>
 * 系统码表 Mapper 接口
 * </p>
 *
 * @author chenjie123123
 * @since 2019-05-27
 */
public interface TSystemCodeTMapper extends BaseMapper<TSystemCodeT> {

    /**
     *
     * @param map
     * @param pagination
     * @return
     */
    List<TSystemCodeT> selectSubSystemByCondition(Map<String, Object> map, Pagination pagination);
}
