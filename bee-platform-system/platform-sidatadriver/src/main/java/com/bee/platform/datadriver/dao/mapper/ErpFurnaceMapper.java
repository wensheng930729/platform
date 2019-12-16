package com.bee.platform.datadriver.dao.mapper;

import com.bee.platform.datadriver.entity.ErpFurnace;

import java.util.List;

import com.baomidou.mybatisplus.mapper.BaseMapper;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;

/**
 * <p>
 * 炉子档案 Mapper 接口
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
public interface ErpFurnaceMapper extends BaseMapper<ErpFurnace> {

	List<ErpFurnace> selectList(EntityWrapper<ErpFurnace> entityWrapper, Pagination pagination);

}
