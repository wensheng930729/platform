package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.business.dto.IndexInitDTO;
import com.bee.platform.business.rq.IndexInitRQ;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.entity.IndexInit;

/**
 * <p>
 * 服务类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-03-20
 */
public interface IndexInitService extends IService<IndexInit> {

	/**
	 * @notes 首页临时数据查询
	 * @Author junyang.li
	 * @Date 10:02 2019/3/20
	 **/
	ResponseResult<IndexInitDTO> findAllNumbers();

	/**
	 * 修改临时定死的数据
	 * 
	 * @return
	 */
	ResponseResult modifyAllNumbers(IndexInitRQ rq);
}
