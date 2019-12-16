package com.bee.platform.user.utils;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.util.CollectionUtils;

import com.bee.platform.user.entity.Departments;

/**
 * 部门工具
 */
public class DepartmentUtil {

	/**
	 * 获取部门子节点
	 * @param list
	 * @param orgId
	 * @param resultList
	 * @return
	 */
	public static List<Integer> getTree(List<Departments>list, Integer id, List<Integer>resultList) {
		 // 查询该部门下面所有直接节点的集合
        List<Departments> departments = list.stream()
        		.filter(dep -> id.equals(dep.getTreeId()) && !id.equals(dep.getId()))
        		.collect(Collectors.toList());
        // 从原来集合中删除这个直接节点的集合的所有元素
        list.removeAll(departments);
        // 如果直接节点的元素不为空，就遍历这个直接节点的元素，并从原始集合剩下的元素中去继续找寻上面直接节点集合的每个元素的直接节点
        if (!CollectionUtils.isEmpty(departments)) {
            departments.forEach(dep -> {
            	resultList.add(dep.getId());
            	getTree(list, dep.getId(), resultList);
             });
        }
		return resultList;
	}
	
	/**
	   * 获取当前部门下的所有子节点的ID的集合
	   * @param all 所有集合
	   * @param deptId 当前部门的ID
	   */
//	  public static List<Integer> getTree(List<Departments> all, Integer deptId, List<Integer> res) {
//	    // 过滤出当前节点的直接子节点
//	    Set<Departments> sets = all.stream().filter(x->deptId.equals(x.getTreeId())).collect(Collectors.toSet());
//	    // 遍历过滤出的直接子节点、并加入到结果集合中去、再做递归去处理直接子节点下的jied
//	    if(sets != null && sets.size() > 0){
//	      // 删除过滤出的集合
//	      all.removeAll(sets);
//	      sets.forEach(b-> {
//	        res.add(b.getId());
//	        getTree(all, b.getId(), res);
//	      });
//	    }
//	    return res;
//	  }
	
//	public static Set<Departments> getTree2(List<Departments> all, Integer deptId, Set<Departments> res) {
//	    Set<Departments> sets = all.stream().filter(x->deptId.equals(x.getTreeId())).collect(Collectors.toSet());
//	    if(sets != null && sets.size() > 0){
//	      all.removeAll(sets);
//	      sets.forEach(b-> {
//	        res.add(b);
//	        getTree2(all, b.getId(), res);
//	      });
//	    }
//	    return res;
//	  }
}
